require 'help'
local docstring = help.docstring

function catch_haskell(result, error_msg)
  if result == '_HASKELLERR' then
    error(error_msg)
  else
    return result
  end
end


--[[ GrLang class ]]

GrLang = docstring{[==[
Superclass for all GrLang values.
All instances of this class are wrappers of Haskell values.
]==],
  functions = {
    'add_node_type',
    'add_edge_type',
    'node_types',
    'edge_types',
    'print_types'
  },
  methods = {
    'to_dot',
    'view'
  }
} .. {__index = {}}

GrLang.node_types = docstring[==[
List the names of all registered node types.
]==] .. function ()
  return catch_haskell(GrLang.native.getNodeTypes())
end

GrLang.edge_types = docstring[==[
List the names of all registered edge types.
]==] .. function ()
  return catch_haskell(GrLang.native.getEdgeTypes())
end

GrLang.print_types = docstring[==[
Print the names of all registered node and edge types.
]==] .. function ()
  for i,t in ipairs(GrLang.node_types()) do
    print('node type ' .. tostring(t))
  end
  for i,t in ipairs(GrLang.edge_types()) do
    print('edge type ' .. tostring(t))
  end
end

GrLang.add_node_type = docstring[==[
Given a name, register a new node type.
]==] .. function (name)
  return catch_haskell(GrLang.native.addNodeType(name))
end

GrLang.add_edge_type = docstring[==[
Given a name and the node types for source and target, register a new edge type.
]==] .. function (name, srcName, tgtName)
  return catch_haskell(GrLang.native.addEdgeType(name, srcName, tgtName))
end

function GrLang.__tostring(value)
  return catch_haskell(GrLang.native.toString(value.index))
end

function GrLang.__eq(value1, value2)
  return value1.index == value2.index
end

function GrLang.__gc(value)
  return catch_haskell(GrLang.native.deallocate(value.index))
end

GrLang.__index.to_dot = docstring[==[
Write the value in the dot format for graph drawing.
Optionally receives a name for the given value.
]==] .. function (value, name)
  return catch_haskell(GrLang.native.toDot(value.index, name or ''))
end

GrLang.__index.view = docstring[==[
Draw the given graph using xdot.
]==] .. function (graph)
  local file_name = '/tmp/verigraph-dot' .. os.date()
  local file = io.open(file_name, 'w')
  file:write(graph:to_dot(''))
  file:close()
  os.execute('xdot \"' .. file_name .. '\"')
end


--[[ Creating subclasses ]]

local function subclass_of_GrLang()
  local class = { __index = {}, __tostring = GrLang.__tostring, __eq = GrLang.__eq, __gc = GrLang.__gc }
  setmetatable(class.__index, { __index = GrLang.__index })
  return class
end

--[[ Graph class ]]

Graph = docstring{[==[
Class for GrLang graphs, subclass of GrLang.

Instances can be constructed as follows:
    Graph[[
      n1 n2 : NodeType
      n1 -:EdgeType1, f g:EdgeType2-> n2
    ]]
]==]} .. subclass_of_GrLang()

setmetatable(Graph, {
  __call = function (cls, str)
    local idx = catch_haskell(Graph.native.parseGraph(str))
    local instance = { index = idx }
    setmetatable(instance, cls)
    return instance
  end
})

--[[ Requiring GrLang files ]]

local function loader(modname, path)
  catch_haskell(GrLang.native.compileFile(path))
  return
end

package.searchers[#package.searchers + 1] =
  function(modname)
    path = package.searchpath(modname, './?.tg;./?.grl')
    if path then
      return loader, path
    end
  end
