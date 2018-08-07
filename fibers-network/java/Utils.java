package eta.fiber.network;

import java.nio.channels.Channel;
import java.nio.channels.SocketChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.SelectableChannel;
import java.nio.channels.ClosedChannelException;
import java.io.IOException;
import eta.runtime.concurrent.Concurrent;
import eta.runtime.stg.StgContext;
import java.io.IOException;


public class Utils{
    public static void setNonBlock(Channel c) throws IOException{
        ((SelectableChannel) c).configureBlocking(false);
    }
    public static void registerRead(StgContext s, Channel c) throws IOException{
        SelectableChannel selectChannel = (SelectableChannel) c;
        selectChannel.register(Concurrent.globalSelector, SelectionKey.OP_READ, s.currentTSO);
    }
    public static void registerWrite(StgContext s, Channel c) throws IOException{
        SelectableChannel selectChannel = (SelectableChannel) c;
        selectChannel.register(Concurrent.globalSelector, SelectionKey.OP_WRITE, s.currentTSO);
    }
    public static void registerAccept(StgContext s, Channel c) throws IOException{
        SelectableChannel selectChannel = (SelectableChannel) c;
        selectChannel.register(Concurrent.globalSelector, SelectionKey.OP_ACCEPT, s.currentTSO);
    }
    public static void registerConnect(StgContext s, Channel c) throws IOException{
        SelectableChannel selectChannel = (SelectableChannel) c;
        selectChannel.register(Concurrent.globalSelector, SelectionKey.OP_CONNECT, s.currentTSO);
    }
}