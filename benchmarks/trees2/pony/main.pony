use "collections"

type Node is (TreeNode | Empty)

primitive Empty
  fun check(): I32 => 0

class TreeNode
  var item: I32
  var left: Node
  var right: Node

  new create(i: I32) =>
    item = i
    left = Empty
    right = Empty

  fun check(): I32 =>
    (left.check() - right.check()) + item

primitive FillTree
  fun apply(i: I32, depth: I32): TreeNode =>
    var queue = Array[TreeNode](((1 << depth.u32()) - 1).usize())
    var result = TreeNode(i)
    queue.push(result)

    var head: I32 = 0
    var nodes: I32 = 0
    var target: I32 = (((1 << depth.u32()) - 1) - 1).i32()
    var it: I32 = i

    while nodes < target do
      it = it << 1

      try
        var n = queue(head.usize())
        head = head + 1
        var l = TreeNode(it - 1)
        var r = TreeNode(it)
        n.left = l
        n.right = r
        queue.push(l)
        queue.push(r)
      end

      nodes = nodes + 2
    end
    result

actor Main
  var env: Env
  var ll: LongLived
  var actors: List[HeavyWork]

  var dones: USize = 128

  new create(e: Env) =>
    env = e
    actors = List[HeavyWork]()
    let n = try env.args(1).i32() else 6 end
    var maxDepth = if 6 > n then 6 else n end

    StretchMemory(env, maxDepth + 1)
    ll = LongLived(env, maxDepth)

    var count: USize = 128
    var depths = List[I32]
    while (depths.size() < count) do
//      env.out.print("stuck") 
      for d in Range[I32](4, maxDepth + 1, 2) do 
        depths.push(d)
      end
    end 
    var temp = depths.size() - count
    if temp > 0 then 
      while temp != 0 do 
        try depths.pop() end
        temp = temp - 1
      end 
    end

    // for depth in Range[I32](4, maxDepth + 1, 2) do
    for depth in depths.values() do 
  //    env.out.print(depth.string())
      var iterations = (16 << (maxDepth - depth).u32()).i32()
      actors.push(HeavyWork(env, iterations, depth, this))
    end
    
  be done() =>
    dones = dones - 1
    if dones == 0 then 
      for a in actors.values() do 
        a.report()
      end
      ll()
    end

type Task is (StretchMemory | LongLived | HeavyWork)

actor StretchMemory
  new create(env: Env, stretchDepth: I32) =>
    env.out.print("Stretch: " + FillTree(0, stretchDepth).check().string())

actor LongLived
  var _env: Env
  var _tree: Node

  new create(env: Env, maxDepth: I32) =>
    _env = env
    _tree = FillTree(0, maxDepth)

  be apply() =>
    _env.out.print("Long lived: " + _tree.check().string())

actor HeavyWork
  var _check: I32 = 0
  var _iterations: I32
  var _depth: I32
  var _env: Env

  new create(env: Env, iterations: I32, depth: I32, main: Main) =>
    _iterations = iterations
    _depth = depth
    _env = env

    for i in Range[I32](0, iterations) do
      this(i)
    end

    main.done()

  be apply(i: I32) =>
    _check = _check 
      + FillTree(i, _depth).check() 
      + FillTree(-i, _depth).check()

  be report() =>
    _env.out.print((_iterations << 1).string() + " trees of depth " +
      _depth.string() + " check: " + _check.string())
