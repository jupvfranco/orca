use "collections"

type Node is (TreeNode | Empty)

primitive Empty
  fun check(): USize => 0

class TreeNode
  var item: USize
  var left: Node
  var right: Node

  new create(i: USize) =>
    item = i
    left = Empty
    right = Empty

  fun check(): USize =>
    (left.check() - right.check()) + item

primitive FillTree
  fun apply(i: USize, depth: USize): TreeNode =>
    var queue = Array[TreeNode](((1 << depth) - 1).usize())
    var result = TreeNode(i)
    queue.push(result)

    var head: USize = 0
    var nodes: USize = 0
    var target: USize = ((1 << depth) - 1) - 1
    var it: USize = i

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

  new create(e: Env) =>
    env = e
    let n = try env.args(1).usize() else 6 end
    var maxDepth = if 6 > n then 6 else n end

    StretchMemory(env, maxDepth + 1)
    let ll = LongLived(env, maxDepth)

    for depth in Range[USize](4, maxDepth + 1, 2) do
      var iterations = 16 << (maxDepth - depth)
      HeavyWork(env, iterations, depth)
    end

    ll()

type Task is (StretchMemory | LongLived | HeavyWork)

actor StretchMemory
  new create(env: Env, stretchDepth: USize) =>
    env.out.print("Stretch: " + FillTree(0, stretchDepth).check().string())

actor LongLived
  var _env: Env
  var _tree: Node

  new create(env: Env, maxDepth: USize) =>
    _env = env
    _tree = FillTree(0, maxDepth)

  be apply() =>
    _env.out.print("Long lived: " + _tree.check().string())

actor HeavyWork
  var _check: USize = 0
  var _iterations: USize
  var _depth: USize
  var _env: Env

  new create(env: Env, iterations: USize, depth: USize) =>
    _iterations = iterations
    _depth = depth
    _env = env

    for i in Range[USize](0, iterations) do
      this(i)
    end

    done()

  be apply(i: USize) =>
    _check = _check + FillTree(i, _depth).check() +
      FillTree(-i, _depth).check()

  be done() =>
    _env.out.print((_iterations << 1).string() + " trees of depth " +
      _depth.string() + " check: " + _check.string())
