require "./net/*"

module Net
  def main
    conn = IRC.new
    conn.connect "irc.rizon.net", 6667
    conn.send "USER walcry walcry walcry :walcry"
    conn.send "NICK walcry"
    conn.send "JOIN #walnut"
    conn.recv
  end
end

include Net

main
