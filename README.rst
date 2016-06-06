=======
Motions
=======

Motions is a MCMC simulation of chromatin movements within a cell nucleus
using a variation of the "Strings and Binders Switch" model.

Installation
------------

Make sure that `CMake`_ >= 3.1 and `Git`_ is installed in your system before proceeding.

`Stack`_ is the easiest way to build and run the simulation.
First you need to install `stack`_. Installation depends on the platform.
Most linux distributions should provide an easy way to get `stack`_ from
repositories. Example installation on Debian 8 x64::

    sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 575159689BEFB442
    echo 'deb http://download.fpcomplete.com/debian jessie main'|sudo tee /etc/apt/sources.list.d/fpco.list
    sudo apt-get update && sudo apt-get install stack -y

Detailed instructions can be found in the `stack documentation`_.

When you have stack you can proceed with building the simulation.
The following commands do everything for you::

    git clone https://github.com/Motions/motions.git
    cd motions
    stack setup
    stack build

Some dependencies require a fair amount of RAM when being built. Our experiences
tell that at least 3 GB is necessary (use swap if needed).

.. _CMake: https://cmake.org/
.. _Git: https://git-scm.com/
.. _stack: http://docs.haskellstack.org/en/stable/README.html
.. _stack documentation: http://docs.haskellstack.org/en/stable/README.html#how-to-install

Running the simulation
----------------------

You can either use stack (if you used stack build)::

    stack exec -- motions

Or simply execute it from the directory containing the executable (the path may vary a little)::

    cd .stack-work/dist/x86_64-linux/Cabal-1.22.4.0/build/motions/
    ./motions

The simulation requires a config file in the `YAML format`_. Example input is provided in the
"example" directory. The "input.yaml" file contains short descriptions of all the parameters.
You can run the example like this::

    cd example
    stack exec -- motions -c input.yaml

There are two essentially different ways of configuring the simulation. You can either
use Motions to generate the initial state randomly or you can load the initial state from a set of files.
Each method corresponds to an entry in the YAML file: "generate" and "load".
Exactly one of these entries must be provided.
The following sections describe each method.

The output of the simulation is given in the `PDB (Protein Data Bank) format`_.
Two PDB files are generated: one contains the lamin binders, the other contains the rest of the output.
One additional ".meta" file is generated. Its description is given in the "using PDB as input" section.

.. _YAML format: http://yaml.org/
.. _PDB (Protein Data Bank) format: http://www.wwpdb.org/documentation/file-format

Running with randomly generated state
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The first way of running Motions is to let it generate a random initial state and start from there.

Motions supports simulation of multiple chains. Each chain bead has an associated energy vector
which describes how strongly the chain interacts with various binders present inside the cell nucleus.

The descriptions of chains and binders are given using:

1. Files in the `BED format`_ to describe the different chain features
   that will later be translated to energy vectors.
2. A list of lengths (integers) of the chains.
3. The simulation resolution.
4. The radius of the cell.
5. A list of numbers (integers) of binders of each type *excluding lamins*.

Example description:

1. File "feat0.bed"::

       chr1	0	9
       chr1	20	29
       chr1	20	29
       chr1	40	49
       chr1	80	89

2. File "feat1.bed"::

       chr1	10	19
       chr1	30	39

3. One length (of the "chr1" chain)::

       100

4. The resolution::

       10

5. The radius::

       10

6. One number - the number of binders corresponding to the "feat1.bed" file::

       1000

For now we only use the required BED fields and we assume that chromosome names have
the form "chrX" or "X", where X is a number, for example "chr5".

The example input describes one chain consisting of 100 base pairs with two different
features appearing on it.
The first feature (described in the "feat0.bed" file) appears on the following ranges:
0-9, 20-29 (twice, which means it's a stronger feature), 40-49, 80-89. The second
feature (described in the "feat1.bed" file) appears on the following ranges: 10-19, 30-39.

The resolution describes how ranges of base pairs are mapped to beads, for example,
if the resolution equals 10, that means the ranges beads 0, 1, ... correspond to base pair
ranges 0-9, 10-19, ... respectively. The strength of a feature in a bead is equal to the number
of feature appearances in the corresponding range.

In the example there are going to be 10 beads. The energy vectors are of length 2 because
there are 2 features. The entries in these vectors are the corresponding feature strengths.
Hence the resulting vectors are::

    [1,0], [0,1], [2,0], [0,1], [1,0], [0,0], [0,0], [0,0], [1,0], [0,0]

The cell nucleus contains binders (floating binders and lamins) which interact with the chains.
Energy vectors are used to determine how each bead interacts with binders of different types.
The number of different binder types is equal to the length of an energy vector
(which is equal to the number of chain features).

At least one type of binder - the lamin - is always present in the simulation. That means
there always has to be at least one BED file even if it's empty (which means no bead
interacts with lamins).

The example input describes that there are 1000 binders of type 1 corresponding to the second entry
of an energy vector (type 0 is the lamin type; it correspnds to the first entry of an energy vector).
The bead numbered 2 will interact with lamins with strength 2 and won't interact with the other
1000 binders, but the bead numbered 1 won't interact with lamins and will interact with the other
1000 binders with strength 1.

.. _BED format: https://genome.ucsc.edu/FAQ/FAQformat.html#format1

Running the simulation using PDB files as input
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The other way is to provide a list of PDB files from which the initial state will be loaded
along with a ".meta" file.
If a provided PDB file contains multiple frames, the first frame will be used as initial state.

The ".meta" file describes how each energy vector, binder and chain occuring in the simulation
is mapped to a string in the PDB format. For example the "residue name" column in a PDB file contains
strings that correspond to energy vectors and binder types.
The simulation generates a ".meta" file each time it is run and it can be used later
to resume the simulation.
Each line in a ".meta" file is one of the following:

1. An energy vector entry which maps a list of natural numbers to a string of length 3, for example::

       EV [0,1] aab

2. A binder type entry which maps a natural number to a string of length 3, for example::

       BT 0 Baa

3. A chain identifier entry which maps a natural number to a single character, for example::

       CH 1 a

4. A chain name entry which maps a natural number to a chain name, for example::

       NM 1 chrom

The resulting mapping must be a bijection if you want to use a ".meta" file as input.
The chain identifiers must be consecutive natural numbers starting from 1.
Every chain must have its name, i. e. the number of NM mappings has to be the same as the number
of CH mappings.

Example PDB files together with a suitable ".meta" file are provided in the "example" directory.

Callbacks
---------

Writing callbacks
~~~~~~~~~~~~~~~~~

New callbacks are added in the config/callbacks file. The project needs to be recompiled after adding a callback.
The file contains one example callback. The full grammar is given::

    <bool_expr> ::= <bool_expr> AND <bool_expr>
        | <bool_expr> OR <bool_expr>
        | NOT <bool_expr>
        | <expr> <comp_operator> <expr>
        | BELONGS ( <node> , <atom_class> )

    <comp_operator> ::= < | <= | > | >= | == | !=

    <atom_class> ::= BEAD
        | BEAD_BINDING_TO <int>
        | BINDER <int>

    <node> ::= X <int>

    <expr> ::= <int>
        | <float>
        | <expr> <operator> <expr>
        | DIST ( <node> , <node> )
        | ENERGY ( <node> , <node> )
        | INT ( <expr> )
        | FLT ( <expr> )
        | ATOM_INDEX ( <node> )
        | CHAIN_INDEX ( <node> )
        | CHROMOSOME ( <node> )
        | MIN ( <expr> , <expr> )
        | MAX ( <expr> , <expr> )

    <operator> ::= + | - | / | * | % | //

    <callback> ::= CALLBACK <string> EVERY ACCEPTED <int> NODES <int> WHERE <bool_expr> COMPUTE <aggregate> <expr>

    <aggregate> ::= SUM | PRODUCT | LIST

Enabling callbacks
~~~~~~~~~~~~~~~~~~

To enable callbacks use the YAML configuration file. The "enabled-callbacks" option contains a list of all
the callbacks you wish to compute during the simulation, e.g.::

    enabled-callbacks:
        - Standard Score
        - Gyration Radius
        - lamin-count
