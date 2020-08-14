# Why treemaps?

Treemaps are an underutilized visualization that are capable of generically summarizing data of many shapes and sizes. To date, they've mostly been used for displaying the files consuming all of your disk space, but with a few tweaks, treemaps can be a flexible tool for exploring and navigating messy data blobs.

Treemaps are space filling. You provide the bounds, and the the treemap algorithm will generate a graphic that uses all of the pixels. This is in contrast to something like pprint, which generates a view of the data that is proportional to the amount of data. Bounding the size of the visual representation has the advantage that treemaps scale gracefully for small to medium sized data.

Treemaps will use as many pixels as are available to represent the underlying data. In general, more pixels means more clarity. However, the treemap performs well even at relatively small sizes.

![Sizes Example](images/sizes-example.gif)

Treemaps are very flexible. They can visualize any data that is tree-like which includes any data that can be represented as JSON or edn. 

# What are treemaps?

> treemapping is a method for displaying hierarchical data using nested figures, usually rectangles.
> {{blockquote-footer}}[wikipedia](https://en.wikipedia.org/wiki/Treemapping){{/blockquote-footer}}

At its heart, constructing a treemap is straightforward. Given some tree-like data and a rectangle, subdivide the rectangle into smaller rectangles for each of the tree's branches and then recursively apply the same algorithm for each branch.

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

The size of each rectangle is proportional to the size of the associated tree node and all of its children. The more descendants a tree node has, the bigger its rectangle. As an additional feature, the function that determines the size of leaves and branches can be parameterized, but for our examples, we will assume all leaves have a size of 1 and the size of a branch is the sum of the leaves under it.

Here's what the process of subdivision looks like.

![basic subdivide](images/basic-subdivide.gif)

You can see that the naive treemap shows some of the structure of the data we're trying to visualize, but many elements of the data's structure aren't revealed in this basic treemap. Next, we'll look at a few tricks for improving our treemaps to capture more elements of our data's structure. The following is by no means an exhaustive list of techniques. In fact, there's tremendous room for experimentation and improvement.


## Improving the traditional treemap

Treemaps are really good at using all the available pixels, but there's still a lot of work left deciding how best to use the pixels given to us. There are several metrics and aspects that are possible to visualize. Let's consider a few.

**Types:** What types are used in the data?
**Shape:** Is the data deep, shallow, thin, wide?
**Cardinality:** How much data is there?
**Constraints:** What properties must the data have for it to be considered valid?

## Types

One of the more obvious improvements is to paint the background of each rectangle with the type of the data it represents. Here's what that looks like:

![Type Background](images/type-background.png)

Great, now we can see the types of our tree. However, there's a little snag in our plan. It turns out that for most JSON in the wild, the data is mostly comprised of just strings and numbers. It's really higher level data types that we would be interested in, but if we're interested in summarizing the data, it might be because don't have a schema handy. Automatically inferring data types is something we can work on, but let's move on to other options for now.

## Depth

One of the issues with just showing types is that it doesn't tell us much about the actual structure of the data. Just from the types, we can't tell how deep or how wide the data is. If we're not using the background color to represent the types in the data, we can use it for depth:

![Depth Background](images/depth-background.png)

Using the color for depth certainly illuminates whether or not our data structure is deep or wide, but it can still be difficult to decipher the structure of the example data. For example, "Which rectangles share the same parent?"

### Grouping

One way to visualize which rectangles share the same parent is to add a little padding around each level.

![Depth Background](images/depth-padding.png)

Awesome. Just a little spacing helps track the different branches of the tree and see which elements share the same parents. However, there are some limitations with using only spacing to show grouping. How much padding should each level of the hierarchy have? Adding too little padding makes the hierarchies less apparent. Adding too much spacing can waste pixels that could otherwise be more effective. The shape of the data will also influence how much padding is necessary. Determining the amount of padding that works well on various different types of data is still an area that needs work.

### Hierarchy Lines

Another way to visualize the shape of data is to simply draw lines from parents to their children. We can even change the color of the line to show what type each collection is.

![Line Depth](images/line-bare-demo.png)

The main drawback of hierarchy lines is that the lines can overlap and obscure descendant rectangles. We can partially alleviate the overlapping issue by reducing the hierarchy line's opacity near the top of the tree. However, for certain data shapes, the lines can still be an issue. Another way to declutter the graphic while still utilizing hierarchy lines is to allow the user to hover over the graphic and only show the hierarchy line of the element that is currently under the mouse.

Below is a visualization of the same data as above, but using the background to show depth and only showing the hierarchy lines when hovering.

![Lines Interactive](images/lines-interactive-shrunk.gif)

## Labels

For small examples it's possible to simply label all of the data.

```edn
{:a [0 1 2 3 4],
 :b [0 1 2 3 4],
 :c [0 1 2 3 4]}
```
![Simple Label Example](images/simple-label-example.png)

## Key path labels

One common form of nesting is maps containing other maps. We can highlight important structural features of the data if we emphasize the map keys in our treemap. 

Below is a treemap of all the public interns of namespaces on treemap-cljs's classpath that start with `clojure.*`.

![Keyed Example](images/keyed-example.png)

As an additional help the user, we allow the user to hover over the data and show the key path that it would take to traverse the data to that leaf node.

![Hover Keyed Example](images/hover-keypath-shrunk.gif)

Not only does showing the key path while hovering help show where the data is situated, we can use the key paths as part of the UI itself. As we hover over the keypath, watch as the area for that subsection of the tree is highlighted in the treemap graphic.

![Hover Box Example](images/keypath-box-hover-shrunk.gif)


# Comparisons with alternatives

There are several tools that help us to gain an intuition for a particular data representation. Let's compare treemaps with other options to see how treemaps can most effectively be used as part of the data exploration toolset.

### Treemaps

Treemaps excel at displaying high level structure that is heirarchical, heterogeneous, and approximately square (ie. the data is about as many layers deep as it is wide). Treemaps struggle with data that is either (wide and shallow) or (thin and deep). 

### pprint

`pprint` excels at small to medium size data, especially if the data fits on a single page. Once the data takes more than a page to `pprint`, then it can obscure the shape and structure of the data.

### Data Browsers

Data browsers like [rebl](https://github.com/cognitect-labs/REBL-distro) excel at scanning through data, but typically only show one level of the data at a time. Many data browsers allow graphical integrations, so hopefully treemaps will be [integrated](https://github.com/phronmophobic/treemap-clj) within data browsers to allow for the "big picture" data summarization that treemaps provide.

### Schemas

Schemas excel at providing an abstract summary of data. Schemas have trouble with deeply nested data, data that are out of sync with the schema, and data that don't have a schema. Additionally, schemas don't usually detail real world usage. They often contain properties that are no longer used or have developed new meaning compared to what the original property name would suggest. Schemas are still very useful and can complement tools that work with concrete instances of data like treemaps, `pprint`, and data browsers.

# Future Work

**More sophisticated layout**
Treemap layout in `treemap-clj` is fairly naive. The layout only considers one level at a time and only uses simple to heuristics to prefer squarish rectangles (aspect ratios close to 1) over long and thin rectangles.

Currently, layout of the treemap rectangles is only done one layer at at time. It should be possible to produce better (for various metrics of better) layouts by considering more than one layer of the tree at a time.

**Non rectangular treemaps**
All `treemap-clj` layouts subdivide rectangles into smaller rectangles. However, the [literature](https://www.uni-konstanz.de/mmsp/pubsys/publishedFiles/NoBr12a.pdf) contains algorithms that subdivide areas into other shapes which could have interesting applications.

**Interactive depth rendering**
Allowing a user to interactively render a treemap up to X level of depth is likely to be an interesting way of exploring a data structure.

**Alternative coloring schemes**
Only two uses of color have been presented as part of `treemap-clj`, depth and data type. Other coloring schemes should be investigated.

**Alternative size functions**
As noted above, all `treemap-clj` implementations use a leaf size of 1. Plugging in different sizing functions would allow the user to emphasize different elements of a data structure.

**Use Layout direction to encode more information**
If you look at other visualization graphics, the directions on the chart typically encode information (eg. a financial chart that goes up and to the right is usually a positive sign). The directions on a treemap don't encode any meaning. It should be possible to place rectangles with certain properties close to either the edges or towards the center to emphasize different qualities of the data.

**Graphic Design**
I'm bad at graphic design and have probably violated innumerable graphic design principles. Incorporating graphic design expertise would greatly increase clarity and legibility.

**Constraints, schemas and specs**
Formal data specifications encode a ton of information. Encoding these specifications into the treemap graphic should increase the information density.

**Zooming in and out**
Just like with geographic maps, it should be possible to zoom in on a part of a treemap to reveal more detail or zoom out to view higher level data features.


# Further Reading


Visualizing business information using generalized treemaps
[https://pure.tue.nl/ws/files/47041749/631721-1.pdf](https://pure.tue.nl/ws/files/47041749/631721-1.pdf)

Visualizing Business Data with Generalized Treemaps
[https://ieeexplore.ieee.org/document/4015431](https://ieeexplore.ieee.org/document/4015431)

Computing Voronoi Treemaps
[https://www.uni-konstanz.de/mmsp/pubsys/publishedFiles/NoBr12a.pdf](https://www.uni-konstanz.de/mmsp/pubsys/publishedFiles/NoBr12a.pdf)

Fast Dynamic Voronoi Treemaps
[https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/isvd.pdf](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/isvd.pdf)
