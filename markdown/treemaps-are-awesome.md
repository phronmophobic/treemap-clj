# Treemaps are awesome!


# What are treemaps?

From [wikipedia](https://en.wikipedia.org/wiki/Treemapping)
> treemapping is a method for displaying hierarchical data using nested figures, usually rectangles. 

An important attribute of treemaps is that they are space filling. You provide the bounds, and the the treemap algorithm will generate a graphic that uses all of the pixels. This is in contrast to something like pprint, which generates a view of the data that is proportional to the amount of data. Bounding the size of the visual representation has the advantage that treemaps scale gracefully for small to medium sized data.

Treemaps are also very flexible. They can visualize any data that is tree-like which includes any data that can be represented as JSON. 

At its heart, the treemap is a very simple algorithm. Given a rectangle, the treemap will subdivide the rectangle into smaller rectangles for each of the tree node's children. The

```clojure
(defn treemap [tree-node rect]
  (if (branch? tree-node)
    (let [child-rects (subdivide rect (children tree-node))]
      (mapcat (fn [child child-rect]
                (treemap child child-rect))
              (children tree-node)
              child-rects))
    [rect]))
```

The size of each rectangle is proportional to the size of the associated tree node and all of its children. The more children and descendants a tree node has, the bigger its rectangle. As an additional feature, the function that determines the size of leaves and branches can be parameterized, but for our examples, we will assume all leaves have a size of 1 and the size of a branch is the sum of the leaves under it.

Here's what the process of subdivision looks like.

![basic subdivide](images/basic-subdivide.gif)

You can see that the treemap shows some of the structure of the data we're trying to visualize, but there's still a lot of the structure that data that isn't revealed in this basic treemap. Next, we'll look at ways at a few tricks for improving our treemaps to capture more elements of the structure of our data. The following is by no means an exhaustive list of techniques. In fact, there's tremendous room for exploration and improvement.


## Improving Treemaps for data for things

Treemaps are really good at using all the available pixels, but there's still a lot of work left deciding how best to use the pixels given to us. 


## Types

One of the most obvious improvements is to paint the background of each rectangle with the type of the data it represents. Here's what that looks like:

![Type Background](images/type-background.png)

Great, now we can see the types of our tree. However, there's a little snag in our plan. It turns out that for most JSON in the wild, the data is mostly comprised of just strings and numbers. It's really higher level data types that we would be interested in, but if we're interested in summarizing the data, it might be because don't  have a schema handy. Automatically inferring data types is something we can work on, but let's move on to other options for now.

## Depth

One of the issues with just showing types is that it doesn't tell us much about the actual structure of the data. Just from the types, we can't tell how deep or how wide the data is. If we're not using the background color to represent the types in the data, we can use it for depth:

![Depth Background](images/depth-background.png)

Using the color for depth certainly illuminates whether or not our data structure is deep or wide, but it can still be difficult to decipher the structure of the example data. For example, "Which rectangles share the same parent?"

### Grouping

One way to visualize which rectangles share the same parent is to add a little padding around each level.

![Depth Background](images/depth-padding.png)

Awesome. Just a little spacing helps track the different branches of the tree and see which elements have share the same parents. However, there are some limitations with using only spacing to show grouping. The issue comes with trying to decide how much spacing to include at each level of the hierarchy. Adding too little padding makes the hiearchies less apparent. Adding too much spacing can waste pixels that could otherwise be more effective. The shape of the data will also influence how much padding makes the graphic the most clear. Determining the best use of padding that works well on various different types of data is still an area that needs work.

### Hierarchy Lines

Another way to visualize the shape of data is to simply draw lines from parents to their children. We can even change the color of the line to show what the type of the collection is.

![Line Depth](images/line-bare-demo.png)

The main drawback of hierarchy lines is that they the lines can obscure the children farther down the tree. We partially alleviate the fact that heirarchy lines overlap other elements by reducing the opacity for lines near the top of the tree. However, for certain data shapes, the lines can still be an issue. Another way to declutter the graphic while still utilizing heirarchy lines to show lineage is to allow the user to user the mouse to over hover the graphic and only show the hiearchy line of the element that is currently being hovered.

Below is a visualization of the same data as above, but using the background to show depth and only showing the heiarchy lines when hovering over a rectangle.

![Lines Interactive](images/lines-interactive-shrunk.gif)

## Labels

For small examples it's also possible to simply label all of the data.

```edn
{:a [0 1 2 3 4],
 :b [0 1 2 3 4],
 :c [0 1 2 3 4]}
```
![Simple Label Example](images/simple-label-example.png)

## Key path labels

One common and useful for of nesting, is maps containing other maps. We can highlight important structural features of the data if we emphasize the map keys in our treemap. 

Below is a treemap of all the public interns of available namespaces that start with `clojure.*`.

![Keyed Example](images/keyed-example.png)

As an additional help the user, we allow the user to hover over the data and show the key path that it would take to traverse the data to that leaf node.

![Hover Keyed Example](images/hover-keypath-shrunk.gif)

Not only does showing the key path while hovering help show where teh data is situated, we can use the key paths as part of the UI itself. As we hover over the keypath, watch as the area for that subsection of the tree is highlighted in the treemap graphic.

![Hover Box Example](images/keypath-box-hover-shrunk.gif)

# Future Work



# References


Visualizing business information using generalized treemaps
[https://pure.tue.nl/ws/files/47041749/631721-1.pdf](https://pure.tue.nl/ws/files/47041749/631721-1.pdf)

Visualizing Business Data with Generalized Treemaps
[https://ieeexplore.ieee.org/document/4015431](https://ieeexplore.ieee.org/document/4015431)

Computing Voronoi Treemaps
[https://www.uni-konstanz.de/mmsp/pubsys/publishedFiles/NoBr12a.pdf](https://www.uni-konstanz.de/mmsp/pubsys/publishedFiles/NoBr12a.pdf)

Fast Dynamic Voronoi Treemaps
[https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/isvd.pdf](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/isvd.pdf)
