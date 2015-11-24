require "socket"

module Net
    class IRC < Connection
        def connect(server : String, port : Int32)
            @server = server
            @port   = port
            @conn   = TCPSocket.new server, port
        end

        def send(message)
            @conn.try do |conn|
                conn.puts message
            end
        end

        def recv
            @conn.try do |conn|
                loop do
                    line = conn.gets
                    puts line
                end
            end
        end
    end
end
