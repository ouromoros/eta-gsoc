package eta.contorl.concurrent.fiber.network;

import java.nio.channels.Channel;
import java.nio.channels.SocketChannel;

public class Utils{
    public static void setNonBlock(Channel c) throws IOException{
        ((SocketChannel) c).configureBlocking(false);
    }
}
