Pickler Visualizer
==================

Based on [code by Andrey Vityuk](https://github.com/avityuk/scala-pickled-visualizer).
I updated it from scala 2.8 to 2.10, put it in an sbt project, and threw in misc enhancements.

You need [graphviz](http://www.graphviz.org) installed.  [brew install graphviz](http://mxcl.github.com/homebrew/) or equivalent, and then:

    # or make up your own class names
    visualize scala.Mutable scala.util.Random 

That will generate .dot files in target/dot. If things are going well,
it might also generate pdf visualizations, and if they're going really
well, it might even display them for you to gawk at.

