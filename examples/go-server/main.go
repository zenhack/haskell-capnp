package main

import (
	"context"
	"net"

	"zombiezen.com/go/capnproto2/rpc"
)

type EchoImpl struct{}

func (EchoImpl) Echo(ctx context.Context, p Echo_echo) error {
	args := p.Args()
	results, err := p.AllocResults()
	if err != nil {
		return err
	}
	query, err := args.Query()
	if err != nil {
		return err
	}
	return results.SetReply(query)
}

func chkfatal(err error) {
	if err != nil {
		panic(err)
	}
}

func main() {
	l, err := net.Listen("tcp", ":4000")
	chkfatal(err)
	for {
		conn, err := l.Accept()
		if err == nil {
			transport := rpc.NewStreamTransport(conn)
			rpcConn := rpc.NewConn(transport, &rpc.Options{
				BootstrapClient: Echo_ServerToClient(EchoImpl{}, nil).Client,
			})
			go func(conn net.Conn) {
				defer conn.Close()
				<-rpcConn.Done()
			}(conn)
		}
	}
}
