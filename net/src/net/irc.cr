require "socket"

module Net
    class IRC < Connection
        def connect(server, port, options)
            # Store details and connect to server.
            @server = server
            @port = port
            @options = options
            @conn = TCPSocket.new server, port

            # Extract IRC specific options.
            nick = options[:nick]
            chan = options[:chan]

            # Send Connection details.
            @conn.try do |conn|
                conn.puts "USER #{nick} #{nick} #{nick} :#{nick}"
                conn.puts "NICK #{nick}"

                if chan.is_a?(Array(String))
                    chan.each do |chan|
                        conn.puts "JOIN #{chan}"
                    end
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
                loop do
                    line = conn.gets
                    puts line
                end
            end
        end
    end
end
