use "collections"
use "time"

actor Main
  var _env: Env
  // var _logs: Array[(USize, Array[U64])]
  var _countReports: USize = 0
  var _maxReports: USize = 0
  var _m: USize = 0
  
  new create(env: Env) =>
    _env = env
    // _logs = Array[Array[U64]](0)
    try 
      let nServers: USize = env.args(1).usize()
      let messages: USize = env.args(2).usize()
      let depthMin: USize = env.args(3).usize()
      let depthMax: USize = env.args(4).usize()
      let start   : U64   = Time.micros()
      
      _m = messages
      _maxReports = nServers
      // _logs = Array[Array[U64]](nServers)

      var depths = Array[USize]()
      var nDepths: USize = 0
      for d in Range(depthMin, depthMax + 1, 2) do
        depths.push(d) 
        nDepths = nDepths + 1
      end      

      for i in Range(0, nServers) do 
        // env.out.print(">>> " + depths(i%nDepths).string())
        Server(messages, depths(i%nDepths), start, this, i)
      end
    else 
      _env.out.print("usage: <number of servers><number of messages><depth>")
    end

  be report(a: Array[U64] iso, id: USize, depth: USize) => 
    var logs: Array[U64] ref = consume a 
    for v in logs.values() do 
      _env.out.print("(" + id.string() + "," 
                         + depth.string() + "," 
                         + v.string() + ")")
    end

    // _logs.push((id, recover ref consume a end))
    // _countReports = _countReports + 1
    // if _countReports == _maxReports then 
      // for l in _logs.values() do 
      //   for v in l.values() do 
      //     _env.out.print(v.string())
      //   end
      //   _env.out.print("==")
      // end 
      // var result = Array[U64]()
      // for i in Range(0, _m) do 
      //   var sum: U64 = 0
      //   for j in Range(0, _maxReports) do 
      //     try sum = sum + _logs(j)(i) end
      //   end
      //   result.push(sum/_maxReports.u64())
      //   _env.out.print((sum/_maxReports.u64()).string())
      // end
    // end

/***** Server side ******/
actor Server 
  var _depth: USize 
  var _logs : Array[U64] iso
  var _main : Main
  var _id   : USize

  new create(m: USize, d: USize, start: U64, main: Main, i: USize) => 
    _depth = d
    _logs  = recover iso Array[U64](m) end
    _main  = main
    _id    = i 
    server_request(m, start)

  be server_request(m: USize, start: U64) =>
    if m >= 1 then 
      FillTree(_depth).check()
      let finish = Time.micros()
      _logs.push(finish - start)
      server_request(m-1, start) 
    else
      _main.report(_logs = recover iso Array[U64]() end, _id, _depth)
    end

/***** Passive stuff *****/
type Node is (TreeNode | Empty)

primitive Empty
  fun check(): USize => 0

class TreeNode
  var left: Node
  var right: Node

  new create() =>
    left = Empty
    right = Empty

  fun check(): USize =>
    1 + (left.check() + right.check())

primitive FillTree
  fun apply(depth: USize): TreeNode =>
    var queue = Array[TreeNode](((1 << depth) - 1).usize())
    var result = TreeNode
    queue.push(result)

    var head: USize = 0
    var nodes: USize = 0
    var target: USize = ((1 << depth) - 1) - 1

    while nodes < target do
      try
        var n = queue(head.usize())
        head = head + 1
        var l = TreeNode
        var r = TreeNode
        n.left = l
        n.right = r
        queue.push(l)
        queue.push(r)
      end
      nodes = nodes + 2
    end
    result

/*
APPROACH:

1. Main actor start up. It:
  (a) Spawns as many server actors as there are cores.
  (b) Sends an `init` message to each server actor with
      the number of messages to process (MSGS) and
      the size of each message (DEPTH) and sends the
      application's start up time S to each actor.

2. Server `init`:
  (a) Creates an array of integers (LOG) of size MSGS
      for keeping track of service times.
  (b) sends a `serve_request` message to itself

3. Server `serve_request`:
  if MSGS > 1,
    (a) perform work on a tree of depth DEPTH
    (b) read the current time E off the system clock
    (c) log (E - S) in LOG's next free slot
    (e) if MSGS > 2, send a `serve_request` message to
        itself, else send LOG to the main actor

The main actor has a pre allocated log array for time stamps and
notes the time stamps as each server actor "reports in". Once all
server actors are done, it calculates the average response time
and the standard deviations.

On a benchmark with no GC-induced pauses, we should see that the
processing times for message i and message i+1 (for each server)
increases linearly, in other words: LOG[i+1] - LOG[i] \approx
LOG[i+2] - LOG[i+1]. In Pony, we should see some gaps due to
garbage collection, but each server only affects itself. With a
STW GC, we should see much more jitter -- GC pauses will be
longer, distrupting processing times across the whole system, but
in-between GC stops, processing times should be uniform.
  */