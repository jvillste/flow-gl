# Component image cache

* for each event batch collect dependables and their states and a
  mapping from node ids to dependables
* to find out which components should be rendered to texture:
  * compare dependable states to states after previous event batch to
    find out changed dependables
  * filter out node ids which dependables have not changed
  * go through the scene graph depth first and do not descent under
    nodes which dependables have not changed

* layout changes may invalidate image caches without changes in the
  node dependables
  * the images are cached by the scene graph node so if the node
    changes the images are rendered again

# cache cleanup

* how to cleanup dependency map in cache?
  * the cache cleans up function application return values in least
    used first order
  * we can periodically go through each funciton application in the
    dependables map and remove keys that are not cached
  * if we would implement the cache our selves we could include the
    dependency map cleanup in the return value cleanup

# node and component ids

The nodes in the scene graph after the component compilation can be
identified without the :call keywords in the node ids. The :call
keywords are only needed to identify the components and their states.

# view call complation cache

Component hierarchy compilation should be cached.

How to keep track of component instances when they are not called on
every frame?

component 1 <- state 1
  component 2 <- state 2
  component 3 <- state 2, state 3


when should scenegraphs be removed from scenegraph cache?

we should track cache hits and applied view calls. After compiltation scenegraphs that are not children of cache hits

what if a view call generates other children than on previous compilation?

  - on each compilation all applied view calls and cache hits are collected.
  - after compilation these are compared to the previous compilation
    and any scene graphs that are not children of applied view calls
    are removed fromt the cache
  - on cache hit, all children of the cached view call are marked as applied

removed from cache When view call is compiled


;; layout
;; view compilation
;; component texture cache
;; event handling
;; rendering
;; how to test scene graph generation and event handlig without rendering
