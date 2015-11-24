require "./net/*"

module Net
  def main
    conn = IRC.new
    conn.connect "irc.rizon.net", 6667, {
        nick: "walcry",
        chan: [
            "#walnut"
        ]
    }

    conn.recv
  end
end

include Net

main
