use "files"
use "collections"

type Tree is (TreeNode | Empty)

primitive Empty

class TreeNode
  var left: Tree
  var right: Tree

  new create(depth: USize) =>
    if depth > 1 then
      left = TreeNode(depth-1)
      right = TreeNode(depth-1)
    else
      left = Empty
      right = Empty
    end

actor Node
  var _next: (Node | None) = None
  var _env: Env
  var _id: USize

  new create(id: USize, env: Env) =>
    _id = id
    _env = env

  be setNext(n: Node) =>
    _next = n

  be apply(msgs: USize, tree: Tree iso) =>
    if msgs == 0 then
      _env.out.print("Done " + _id.string())
    else
      try (_next as Node).apply(msgs-1, consume tree) end
    end

actor Main
  var _actors: USize // number of actors
  var _depth : USize  // depth of tree
  var _rounds: USize // number of rounds

  new create(env: Env) =>
    _actors = 0
    _depth = 0
    _rounds = 0
    if _arguments(env) then
      _createAndStart(env)
    else
      env.out.print("wrong number of arguments")
    end

  be _createAndStart(env: Env) =>
    let messages = _actors * _rounds
    var nodes = Array[Node](_actors)
    // spawn actors
    for i in Range(0, _actors) do nodes.push(Node(i, env)) end
    // setup ring
    for i in Range(0, _actors-1) do
      try nodes(i).setNext(nodes(i+1)) end
    end
    try nodes(_actors - 1).setNext(nodes(0)) end
    // start the computation
    let tree: TreeNode iso = recover iso TreeNode(_depth) end
    try nodes(0)(messages, consume tree) end

  fun ref _arguments(env: Env): Bool =>
    try
      _actors = env.args(1).usize()
      _depth = env.args(2).usize()
      _rounds = env.args(3).usize()
      true
    else
      false
    end
