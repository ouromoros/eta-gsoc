package eta.fiber.network;

import java.nio.channels.Channel;
import java.nio.channels.SocketChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.SelectableChannel;
import java.io.IOException;
import eta.runtime.concurrent.Concurrent;
import eta.runtime.stg.StgContext;

public class Utils{
    public static void setNonBlock(Channel c) throws IOException{
        ((SocketChannel) c).configureBlocking(false);
    }
    public static void waitAccept(StgContext s, Channel c){
        int valid = ((SelectableChannel) c).validOps();
        System.out.println(valid&SelectionKey.OP_ACCEPT);
        Concurrent.waitAccept(s, c);
    }
}