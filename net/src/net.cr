require "./net/*"
require "./config.cr"

module Net
  def main
    server = Array(Connection).new
    config = File.read("config")
    config = Config::Configuration.from_json(config)

    config.irc.each do |irc|
        puts "Connecting to: #{irc.addr}"
        conn = IRC.new
        conn.connect irc
        server << conn
    end

    config.xmpp.each do |xmpp|
        conn = XMPP.new
        conn.connect xmpp
        server << conn
    end

    sockets = server.map { |x| x.conn }
    sockets = sockets.compact

    loop do
        available = IO.select(sockets)
        available.each do |socket|
            conn = server.find { |x| x if x.conn == socket }
            data = conn.recv if conn
            puts data if data
        end
    end
  end
end

include Net

main
