require "./net/*"
require "./config.cr"

module Net
    def main
        puts "Initializing"
        server = Array(Connection).new
        config = File.read("config")
        config = Config::Configuration.from_json(config)

        config.irc.each do |irc|
            conn = IRC.new
            conn.connect irc
            server << conn
        end

        config.xmpp.each do |xmpp|
            conn = XMPP.new
            conn.connect xmpp
            server << conn
        end

        puts "Connections:"
        server.each do |s|
            puts " Server #{s}"
        end

        sockets = server.map { |x| x.conn }
        sockets = sockets.compact

        puts "Sockets:"
        sockets.each do |s|
            puts " Sockets #{s}"
        end

        puts "Sockets -> Connections"
        sockets.each do |s|
            l = server.find do |x|
                puts "x.conn == s: #{x.conn == s}"
                x.conn == s
            end
            puts " #{s} -> #{l}"
        end

        loop do
            available = IO.select(sockets)
            available.each do |socket|
                puts ""
                puts " Ready #{socket.class}"
                conn = server.find(0) do |x|
                    puts "x.conn == s: #{x.conn == socket}"
                    x.conn == socket
                end

                puts " Socket: #{conn.class}"

                if conn.is_a?(Connection)
                    data = conn.recv
                    puts data if data
                end

                exit
                puts ""
            end
        end
    end
end

include Net

main
