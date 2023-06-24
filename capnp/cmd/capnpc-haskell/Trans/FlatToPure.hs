{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Trans.FlatToPure (cgrToFiles) where

import Control.Monad.State (State, evalState, get, put)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Word
import qualified IR.Common as C
import qualified IR.Flat as Flat
import qualified IR.Name as Name
import qualified IR.Pure as Pure

type IFaceMap = M.Map Word64 Pure.Interface

-- | Because of pervasive use of DuplicateRecordFields, we can't easily
-- just use the auto-generated `name` accessor function... so we define
-- this wrapper.
nodeName :: Flat.Node -> Name.CapnpQ
nodeName Flat.Node {name} = name

cgrToFiles :: Flat.CodeGenReq -> [Pure.File]
cgrToFiles Flat.CodeGenReq {allNodes, reqFiles} =
  map oneFile reqFiles
  where
    allInterfaces = concatMap (convertInterface ifaceMap) allNodes
    ifaceMap =
      M.map
        (\iface -> iface {Pure.ancestors = collectAncestors iface})
        $ M.fromList
          [ (interfaceId, iface)
            | iface@Pure.IFace {interfaceId} <- allInterfaces
          ]
    oneFile Flat.File {nodes, fileId, fileName} =
      Pure.File
        { fileId,
          fileName,
          decls = concatMap (nodeToDecls ifaceMap) nodes,
          reExportEnums = concatMap nodeToReExports nodes,
          usesRpc = not $ null [() | Flat.Node {union_ = Flat.Interface {}} <- nodes]
        }

convertInterface :: IFaceMap -> Flat.Node -> [Pure.Interface]
convertInterface
  ifaceMap
  Flat.Node
    { name,
      typeParams,
      nodeId,
      union_ = Flat.Interface {methods, supers}
    } =
    let result =
          Pure.IFace
            { name,
              typeParams = [param {C.paramScope = name} | param <- typeParams],
              interfaceId = nodeId,
              methods =
                [ Pure.Method
                    { name,
                      paramType = C.bothMap nodeName paramType,
                      resultType = C.bothMap nodeName resultType
                    }
                  | Flat.Method {name, paramType, resultType} <- methods
                ],
              supers =
                map
                  ( \(C.InterfaceType Flat.Node {nodeId} brand) ->
                      ( ifaceMap M.! nodeId,
                        fmap nodeName brand
                      )
                  )
                  supers,
              ancestors = collectAncestors result -- See Note [Ancestors]
            }
     in [result]
convertInterface _ _ = []

-- | Collect the of the ancestors of an interface, not including itself. Avoids using
-- the 'ancestors' field, as this is used to compute that field in the first place.
-- See Note [Ancestors]
collectAncestors :: Pure.Interface -> [(Pure.Interface, Pure.Brand)]
collectAncestors Pure.IFace {supers} =
  concat $ evalState (traverse go supers) S.empty
  where
    go :: (Pure.Interface, Pure.Brand) -> State (S.Set Word64) [(Pure.Interface, Pure.Brand)]
    go (iface@Pure.IFace {interfaceId, supers}, brand) = do
      seen <- get
      if interfaceId `S.member` seen
        then pure []
        else
          ( do
              put (S.insert interfaceId seen)
              xs <- concat <$> traverse go supers
              pure $ (iface, brand) : xs
          )

nodeToReExports :: Flat.Node -> [Name.LocalQ]
nodeToReExports Flat.Node {name = Name.CapnpQ {local}, union_ = Flat.Enum _} = [local]
nodeToReExports _ = []

unionToDecl :: Bool -> Name.LocalQ -> Name.LocalQ -> [Name.UnQ] -> [Flat.Variant] -> Pure.Decl
unionToDecl firstClass cerialName local typeParams variants =
  Pure.DataDecl
    Pure.Data
      { typeName = local,
        typeParams,
        cerialName,
        def =
          Pure.Sum
            [ Pure.Variant
                { name = variantName,
                  arg = case fieldLocType of
                    C.VoidField ->
                      -- If the argument is void, just have no argument.
                      Nothing
                    _ ->
                      Just (Pure.type_ (fieldToField field))
                }
              | Flat.Variant
                  { field =
                      field@Flat.Field
                        { fieldName = Name.CapnpQ {local = variantName},
                          fieldLocType
                        }
                  } <-
                  variants
            ],
        firstClass
      }

nodeToDecls :: IFaceMap -> Flat.Node -> [Pure.Decl]
nodeToDecls ifaceMap Flat.Node {name = name@Name.CapnpQ {local}, nodeId, union_, typeParams} =
  let typeParams' = map C.paramName typeParams
   in case union_ of
        Flat.Enum _ ->
          -- Don't need to do anything here, since we're just re-exporting the
          -- stuff from the raw module.
          []
        Flat.Interface {} ->
          [Pure.IFaceDecl (ifaceMap M.! nodeId)]
        Flat.Struct {isGroup, fields = [], union = Just Flat.Union {variants}} ->
          -- It's just one big union; skip the outer struct wrapper and make it
          -- a top-level sum type.
          [unionToDecl (not isGroup) local local typeParams' variants]
        -- Flat.Struct{ isGroup=True, union=Nothing } -> [] -- See Note [Collapsing Groups]
        Flat.Struct {isGroup, fields, union} ->
          Pure.DataDecl
            Pure.Data
              { typeName = local,
                typeParams = typeParams',
                cerialName = local,
                def =
                  Pure.Product $
                    map fieldToField fields
                      ++ case union of
                        Nothing ->
                          []
                        Just _ ->
                          [ Pure.Field
                              { name = "union'",
                                type_ =
                                  C.CompositeType $
                                    C.StructType
                                      (Name.mkSub name "")
                                      ( C.ListBrand $
                                          map
                                            (C.PtrParam . fmap nodeName)
                                            typeParams
                                      )
                              }
                          ],
                firstClass = not isGroup
              }
            : case union of
              Just Flat.Union {variants} ->
                -- Also make a type that's just the union, but give it the
                -- same cerialName:
                [unionToDecl False local (Name.mkSub local "") typeParams' variants]
              Nothing ->
                []
        Flat.Constant {value} ->
          [ Pure.ConstDecl
              Pure.Constant
                { name = local,
                  value = C.bothMap nodeName value
                }
          ]
        Flat.Other -> []

fieldToField :: Flat.Field -> Pure.Field
fieldToField Flat.Field {fieldName, fieldLocType} =
  Pure.Field
    { name = Name.getUnQ fieldName,
      type_ = C.bothMap nodeName (C.fieldType fieldLocType)
    }

-- Note [Collapsing Groups]
-- ========================
--
-- If the argument to a union data constructor is a group, then the fields
-- never exist on their own, so it makes for a nicer API to just collapse
-- the fields directly into the variant, rather than creating an auxiliary
-- struct type.
--
-- However, this is only safe to do if the group does not itself have an
-- anonymous union. The reason for this is that otherwise we could end up
-- with two variants with a field "union'" but different types, which the
-- compiler will reject.
--
-- So the rule is, if a Field.Struct node is a group, and it does not itself
-- have an anonymous union:
--
-- 1. Don't generate a type for it.
-- 2. Collapse its fields into the data constructor for the union.
--
-- Note that we actually depend on (1) to avoid name collisions, since
-- otherwise both the data constructor for the anonymous union and the
-- data constructor for the group will be the same.

-- Note [Ancestors]
-- ================
--
-- When constructing a Pure.Interface, we need to fill in both the immediate
-- superclasses and the full transitive list of ancestors. Strictly speaking
-- the latter is redundant and can be computed from the former -- which is
-- what we do, but we store it so that the PureToHaskell phase doesn't need to
-- worry about it.
--
-- The way we compute this is a bit subtle, in that it involves tying the knot;
-- The 'collectAncestors' function expects the interface as an argument, and
-- computes the correct value of ancestors field without reading it. We then
-- use this function when creating the interface value in the first place.
