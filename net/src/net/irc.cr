require "socket"

module Net
    class IRC < Connection
        def connect(options)
            # Extract options.
            addr = options.addr
            port = options.port
            nick = options.nick
            chan = options.chan as Array(String)

            # Connect.
            @opts = options
            @conn = TCPSocket.new addr, port

            # Send Connection details.
            @conn.try do |conn|
                conn.puts "USER #{nick} #{nick} #{nick} :#{nick}"
                conn.puts "NICK #{nick}"

                chan.each do |chan|
                    conn.puts "JOIN #{chan}"
                end
            end
        end

        def send(message)
            @conn.try do |conn|
                conn.puts message
            end
        end

        def recv
            @conn.try do |conn|
                conn.gets
            end
        end

        def conn
            @conn
        end
    end
end
