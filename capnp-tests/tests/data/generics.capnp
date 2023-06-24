@0xb6421fb8e478d144;
# Schema used for testing generics support.

struct Maybe(T) {
  union {
    nothing @0 :Void;
    just @1 :T;
  }
}

struct Either(A, B) {
  union {
    left @0 :A;
    right @1 :B;
  }
}

struct Pair(A, B) {
  fst @0 :A;
  snd @1 :B;
}

struct Nested(T) {
  struct SomeStruct {
    value @0 :T;
  }

  interface SomeInterface {
    method @0 (arg :T) -> (result :T);
  }
}

struct Specialized(T) {
  either @0 :Either(Text, T);
  nestedStruct @1 :Nested(Data).SomeStruct;
}

struct HasGroup(T) {
  theGroup :group {
    value @0 :T;
  }
}
