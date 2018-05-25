package eta.fiber.network;

import java.nio.channels.Channel;
import java.nio.channels.SocketChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.SelectableChannel;
import java.io.IOException;
import eta.runtime.concurrent.Concurrent;
import eta.runtime.stg.StgContext;
import java.io.IOException;


public class Utils{
    public static void setNonBlock(Channel c) throws IOException{
        ((SelectableChannel) c).configureBlocking(false);
    }
    public static void waitAccept(StgContext s, Channel c){
        Concurrent.waitAccept(s, c);
    }
    public static void waitConnect(StgContext s, Channel c){
        Concurrent.waitConnect(s, c);
    }
    public static void waitRead(StgContext s, Channel c){
        Concurrent.waitWrite(s, c);
    }
    public static void waitWrite(StgContext s, Channel c){
        Concurrent.waitRead(s, c);
    }
}