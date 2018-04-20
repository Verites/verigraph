require 'help'
local docstring = help.docstring

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

local function newGrLang(class, idx)
  local instance = { index = idx }
  setmetatable(instance, class)
  return instance
end

GrLang.node_types = docstring[==[
List the names of all registered node types.
]==] .. function ()
  return GrLang.native.getNodeTypes()
end

GrLang.edge_types = docstring[==[
List the names of all registered edge types.
]==] .. function ()
  return GrLang.native.getEdgeTypes()
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
  return GrLang.native.addNodeType(name)
end

GrLang.add_edge_type = docstring[==[
Given a name and the node types for source and target, register a new edge type.
]==] .. function (name, srcName, tgtName)
  return GrLang.native.addEdgeType(name, srcName, tgtName)
end

function GrLang.__tostring(value)
  return GrLang.native.toString(value.index)
end

function GrLang.__eq(value1, value2)
  return value1.index == value2.index or GrLang.native.equals(value1.index, value2.index)
end

function GrLang.__gc(value)
  return GrLang.native.deallocate(value.index)
end

GrLang.__index.to_dot = docstring[==[
Write the value in the dot format for graph drawing.
Optionally receives a name for the given value.
]==] .. function (value, name)
  return GrLang.native.toDot(value.index, name or '')
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

local function subclass_of_GrLang(factory)
  local class = { __index = {}, __tostring = GrLang.__tostring, __eq = GrLang.__eq, __gc = GrLang.__gc }

  -- Set up the inheritance of GrLang methods
  setmetatable(class.__index, { __index = GrLang.__index })

  -- Set up the class constructor
  setmetatable(class, {
    __call = factory or function (cls, str)
      local idx = cls.native.parse(str)
      return newGrLang(cls, idx)
    end
  })

  return class
end

-- Creates a method that saves its result to avoid recomputing it.
local function memoizing(field_name, fn)
  return function(object)
    if object[field_name] == nil then
      object[field_name] = fn(object)
    end
    return object[field_name]
  end
end


--[[ HsList class ]]

HsListIterator = {}

function HsListIterator.__gc(list)
  HsListIterator.native.deallocate(list.index)
end

function makeListIterator(listIdx, itemFactory)
  local function next(listIter)
    if HsListIterator.native.hasNextItem(listIter.index) then
      return itemFactory(HsListIterator.native.getNextItem(listIter.index))
    end
  end

  local listIter = {index = listIdx}
  setmetatable(listIter, HsListIterator)
  return next, listIter
end

--[[ Graph class ]]

Graph = docstring{[==[
Class for GrLang graphs, subclass of GrLang.

Instances can be constructed as follows:
    Graph [[
      n1 n2 : NodeType
      n1 -:EdgeType1, f g:EdgeType2-> n2
    ]]
]==],
  methods = { 
    'identity',
    'morphisms_to',
    'subgraphs', 'quotients', 
    'disjoint_union', 'overlappings_with' 
  }
} .. subclass_of_GrLang()

Graph.__index.identity = docstring "Returns the identity morphism"
  .. memoizing('__identity', function(graph)
    local idx = Graph.native.identity(graph.index)
    return newMorphism(idx, graph, graph)
  end)

Graph.__index.morphisms_to = docstring [==[
Iterate over all morphisms between two graphs.

The call `G:morphisms_to(H, kind)` returns an iterator
for all morphisms from G to H. The optional kind parameter
may be one of 'all', 'monic', 'epic' or 'iso', defaulting
to 'all'.
]==] .. function(G, H, kind)
  local listIdx = Graph.native.findMorphisms(kind, G.index, H.index)
  return makeListIterator(listIdx, function(idx) return newMorphism(idx, G, H) end)
end

Graph.__index.subgraphs = docstring 'Iterate over all subgraphs of the graph.'
.. function(G)
  local listIdx = Graph.native.findAllSubobjectsOf(G.index)
  return makeListIterator(listIdx, function(idxDom, idxMorph)
    local dom = newGrLang(Graph, idxDom)
    return newMorphism(idxMorph, dom, G)
  end)
end

Graph.__index.quotients = docstring 'Iterate over all quotients of the graph.'
.. function(G)
  local listIdx = Graph.native.findAllQuotientsOf(G.index)
  return makeListIterator(listIdx, function(idxCod, idxMorph)
    local cod = newGrLang(Graph, idxCod)
    return newMorphism(idxMorph, G, cod)
  end)
end

Graph.__index.disjoint_union = docstring [==[
Produce the (injections of) the disjoint union for the given graphs.

The call `G:disjoint_union(H)` produces `f, g`, where f is the injection
of G into the disjoint union, and g the injection of H.
]==]
.. function(G, H)
  local idxObj, idxInjG, idxInjH = Graph.native.calculateCoproduct(G.index, H.index)
  local disjUnion = newGrLang(Graph, idxObj)
  return newMorphism(idxInjG, G, disjUnion), newMorphism(idxInjH, H, disjUnion)
end

Graph.__index.overlappings_with = docstring [==[
Iterate over the overlappings of the given graphs, that is, over quotients of their disjoint unions.

The call `G:overlappings_with(H, kind)` returns an iterator for all overlappings of G and H
with embeddings of the given kind. The optional kind parameter may be one of 'all', 'monic',
'epic' or 'iso', defaulting to 'all'. 
]==] .. function (G, H, kind)
  local idx = Graph.native.findJointSurjections(G.index, kind, H.index, kind)
  return makeListIterator(idx, function(idxCod, idxEmbG, idxEmbH)
    local cod = newGrLang(Graph, idxCod)
    return newMorphism(idxEmbG, G, cod), newMorphism(idxEmbH, H, cod)
  end)
end

--[[ Morphism class ]]

Morphism = docstring{[==[
Class for GrLang morphisms, subclass of GrLang.

Instances can be constructed as follows:
    Morphism(domain, codomain) [[
      n1 -> n2 -- indicate that element n1 is mapped to n2
      e1 e2 -> e3 -- you can map multiple elements at once
    ]]

Note that morphisms can be composed with the concatenation
operator, that is, `f .. g` returns the composite of `f` and `g`
when the domain of `f` is the same as the codomain of `g`.
]==],
  methods = {
    'dom', 'cod', '..',
    'is_monic', 'is_epic', 'is_iso',
    'pullback', 'pushout', 
    'equalize_with', 'coequalize_with',
    'pushout_complement', 'has_pushout_complement', 
    'initial_pushout',
    'commutative_overlappings_with'
  },
  functions = {
    'subobject_inter', 'subobject_union'
  }
} .. subclass_of_GrLang(
  function (cls, domain, codomain)
    return function (str)
      local idx = cls.native.parse(domain.index, codomain.index, str)
      return newMorphism(idx, domain, codomain)
    end
  end)

function newMorphism(idx, domain, codomain)
  local result = newGrLang(Morphism, idx)
  result.__domain = domain
  result.__codomain = codomain
  return result
end

Morphism.__index.dom = docstring "Returns the domain graph."
  .. function(morphism) return morphism.__domain end

Morphism.__index.cod = docstring "Returns the codomain graph."
  .. function(morphism) return morphism.__codomain end
 
Morphism.__concat = function(m1, m2)
  if m1.__domain ~= m2.__codomain then
    error("Given morphisms are not composable.")
  else
    local idx = Morphism.native.compose(m1.index, m2.index)
    return newMorphism(idx, m2.__domain, m1.__codomain)
  end
end

Morphism.__index.is_monic = 
  memoizing('__monic', function(morphism)
    return Morphism.native.isMonic(morphism.index)
  end)

Morphism.__index.is_epic = 
  memoizing('__epic', function(morphism)
    return Morphism.native.isEpic(morphism.index)
  end)

Morphism.__index.is_iso = 
  memoizing('__iso', function(morphism)
    return Morphism.native.isIsomorphism(morphism.index)
  end)

Morphism.__index.pullback = docstring [==[
Given another morphism with same codomain, computes their pullback.

Given X -f-> Z <-g- Y with pullback X <-h- W -k-> Y,
the call `f:pullback(g)` returns `h, k`.
]==]
  .. function (f, g)
    if f.__codomain ~= g.__codomain then
      error('Given morphisms are not a span.')
    end
    local objIdx, ffIdx, ggIdx = Morphism.native.calculatePullback(f.index, g.index)
    local dom = newGrLang(Graph, objIdx)
    return newMorphism(ggIdx, dom, f:dom()), newMorphism(ffIdx, dom, g:dom())
  end

Morphism.__index.pushout = docstring [==[
Given another morphism with same domain, computes their pushout.

Given X <-f- W -g-> Y with pushout X -h-> Z <-k- Y,
the call `f:pushout(g)` returns `h, k`.
]==] .. function (f, g)
  if f.__domain ~= g.__domain then
    error('Given morphisms are not a cospan.')
  end
  local objIdx, ffIdx, ggIdx = Morphism.native.calculatePushout(f.index, g.index)
  local cod = newGrLang(Graph, objIdx)
  return newMorphism(ggIdx, f:cod(), cod), newMorphism(ffIdx, g:cod(), cod)
end

Morphism.__index.equalize_with = docstring [==[
Given another parallel morphism, computes their equalizer.
]==] .. function (f, g)
  if f.__domain ~= g.__domain or f.__codomain ~= g.__codomain then
    error('Given morphisms are not parallel.')
  end
  local objIdx, morphIdx = Morphism.native.calculateEqualizer(f.index, g.index)
  local dom = newGrLang(Graph, objIdx)
  return newMorphism(morphIdx, dom, f:dom())
end

Morphism.__index.coequalize_with = docstring [==[
Given another morphism, computes their coequalizer.
]==] .. function (f, g)
  if f.__domain ~= g.__domain or f.__codomain ~= g.__codomain then
    error('Given morphisms are not parallel.')
  end
  local objIdx, morphIdx = Morphism.native.calculateCoequalizer(f.index, g.index)
  local cod = newGrLang(Graph, objIdx)
  return newMorphism(morphIdx, f:cod(), cod)
end

Morphism.__index.pushout_complement = docstring [==[
Given morphisms A -f-> B -g-> C with monic f, if there exists a pushout complement
A -h-> D -k-> C then the call `f:pushout_complement(g)` returns `h, k`. Otherwise
it returns nothing.
]==] .. function(f,g)
  if not f:has_pushout_complement(g) then
    return
  end

  local objIdx, ggIdx, ffIdx = Morphism.native.calculatePushoutComplementAlongM(f.index, g.index)
  local D = newGrLang(Graph, objIdx)
  return newMorphism(ggIdx, f:dom(), D), newMorphism(ffIdx, D, g:cod())
end

Morphism.__index.has_pushout_complement = docstring [==[
Given morphisms A -f-> B -g-> C with monic f, if there exists a pushout complement
A -h-> D -k-> C then the call `f:has_pushout_complement(g)` returns true, otherwise
it returns false.
]==] .. function (f,g)
  if not f:is_monic() then
    error('First given morphism is not a mono, pushout complements only available for monos.')
  end
  if f.__codomain ~= g.__domain then
    error('Given morphisms are not composable as required for pushout complements.')
  end
  return Morphism.native.hasPushoutComplementAlongM(f.index, g.index)
end

Morphism.__index.initial_pushout = docstring [==[
Calculates the initial pushout of a morphism.

The call `f:initial_pushout()` returns `b, ff, c`
where b is the boundary morphism, c is the context morhpism
and ff the remaining morphism of the pushout square.
]==]
  .. function(f)
    local bObjIdx, cObjIdx, bIdx, ffIdx, cIdx =
      Morphism.native.calculateInitialPushout(f.index)
    local B, C = newGrLang(Graph, bObjIdx), newGrLang(Graph, cObjIdx)
    return
      newMorphism(bIdx, B, f:dom()),
      newMorphism(ffIdx, B, C),
      newMorphism(cIdx, C, f:cod())
  end

Morphism.__index.commutative_overlappings_with = docstring [==[
Given morphisms X <-f- W -g-> Y, the call `f:commutative_overlappings_with(g, [kind])` iterates
over all spans X -h-> Z <-k- Y making a commutative square, such that h and k are of the given
kind.

The optional kind argument may be one of 'all', 'monic', 'epic' or 'iso'. 
]==] .. function (f, g, kind)
  if f.__domain ~= g.__domain then
    error('The given morphisms do not share a domain.')
  end
  local listIdx = Morphism.native.findJointSurjectionSquares(kind, f.index, kind, g.index)
  return makeListIterator(listIdx, function(idxCod, idxFf, idxGg)
    local cod = newGrLang(Graph, idxCod)
    return newMorphism(idxGg, f:cod(), cod), newMorphism(idxFf, g:cod(), cod)
  end)
end

Morphism.subobject_inter = docstring "Given two monomorphisms with same codomain, calculate their intersection."
  .. function (a, b)
    if not a:is_monic() or not b:is_monic() then
      error("The given morphisms are not all monic.")
    end
    if a.__codomain ~= b.__codomain then
      error("The given monos don't share a codomain.")
    end
    local objIdx, morphIdx = Morphism.native.subobjectIntersection(a.index, b.index)
    local C = newGrLang(Graph, objIdx)
    return newMorphism(morphIdx, C, a:cod())
  end

Morphism.subobject_union = docstring "Given two monomorphisms with same codomain, calculate their union."
  .. function (a, b)
    if not a:is_monic() or not b:is_monic() then
      error("The given morphisms are not all monic.")
    end
    if a.__codomain ~= b.__codomain then
      error("The given monos don't share a codomain.")
    end
    local objIdx, morphIdx = Morphism.native.subobjectUnion(a.index, b.index)
    local C = newGrLang(Graph, objIdx)
    return newMorphism(morphIdx, C, a:cod())
  end

--[[ Rule class ]]

Rule = docstring{[==[
Class for GrLang rules, subclass of GrLang.

Instances can be constructed as follows:
    Rule [[
      match {
        n1 n2 : NodeType
        n1 -:EdgeType1, f g:EdgeType2-> n2
      }
      forbid n2 -:EdgeType1-> n1
      delete f
      clone n2 as n3
      -- also create and join
    ]]
]==],
  methods = {
    'lhs', 'rhs', 'interface', 'l', 'r',
    'find_matches'
  }
} .. subclass_of_GrLang()

Rule.__index.lhs = docstring "Get the LHS graph of the rule"
  .. memoizing('__lhs', function(rule)
    local idx = Rule.native.getLeftObject(rule.index)
    return newGrLang(Graph, idx)
  end)

Rule.__index.rhs = docstring "Get the RHS graph of the rule"
  .. memoizing('__rhs', function (rule)
    local idx = Rule.native.getRightObject(rule.index)
    return newGrLang(Graph, idx)
  end)

Rule.__index.interface = docstring "Get the interface graph of the rule"
  .. memoizing('__interface', function (rule)
    local idx = Rule.native.getInterface(rule.index)
    return newGrLang(Graph, idx)
  end)

Rule.__index.l = docstring "Get the left morphism of the rule"
  .. memoizing('__l', function(rule)
    local idx = Rule.native.getLeftMorphism(rule.index)
    return newMorphism(idx, rule:interface(), rule:lhs())
  end)

Rule.__index.r = docstring "Get the right morphism of the rule"
  .. memoizing('__r', function(rule)
    local idx = Rule.native.getRightMorphism(rule.index)
    return newMorphism(idx, rule:interface(), rule:rhs())
  end)

Rule.__index.find_matches = docstring[==[
Find all applicable matches into the given graph.
]==] .. function(rule, graph)
  if type(graph) ~= 'table' or getmetatable(graph) ~= Graph then
    error('Rule.find_matches must be called with a graph.')
  end
  return Rule.native.findMatches(rule.index, graph.index)
end

--[[ Requiring GrLang files ]]

local function loader(modname, path)
  GrLang.native.compileFile(path)
  return
end

-- Memoize the search path using a weak table, so no memory leak
local search_path = {}
setmetatable(search_path, {__mode = 'k'})

local function get_search_path()
  if not search_path[package.path] then
    local path = ''
    
    local i
    for i in package.path:gmatch('[^;]*;') do
      if i:match('%?%.lua') then
        path = path .. i:gsub('%?%.lua', '?.tg') .. i:gsub('%?%.lua', '?.grl')
      end
    end

    i = package.path:match('[^;]*$')
    if i:match('%?%.lua') then
      path = path .. i:gsub('%?%.lua', '?.tg') .. ';' .. i:gsub('%?%.lua', '?.grl')
    end

    search_path[package.path] = path
  end
  return search_path[package.path]
end

package.searchers[#package.searchers + 1] =
  function(modname)
    path = package.searchpath(modname, get_search_path())
    if path then
      return loader, path
    end
  end