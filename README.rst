=======
Motions
=======

Motions is a MCMC simulation of chromatin movements within a cell nucleus
using a variation of the "Strings and Binders Switch" model.

Installation
------------

Make sure that `CMake`_ >= 3.1 and `Git`__ is installed in your system before proceeding.

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

Motions supports simulation of multiple chains. Each chain bead has an associated energy vector
which describes how strongly the chain interacts with various binders present inside the cell nucleus.
The descriptions of chains are given using:

1. Files in the `BED format`_ to describe the different chain features
   that will later be translated to energy vectors.
2. One file containing lengths of chains given by a line of integers
   separated with spaces.
3. The simulation resolution given by a command line argument.

Example input containing chain descriptions:

1. File "feat0.bed"::

       chr1	0	9
       chr1	20	29
       chr1	20	29
       chr1	40	49
       chr1	80	89

2. File "feat1.bed"::

       chr1	10	19
       chr1	30	39

3. File "lengths"::

       100

4. The resolution command line argument::

       10

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

The numbers of binders of each type (excluding lamins) are given in a file.
Example file "binders"::

    50

describes that there are 50 binders of type 1 corresponding to the second entry of an energy vector
(type 0 is the lamin type; it correspnds to the first entry of an energy vector). Following the example,
the bead numbered 2 will interact with lamins with strength 2 and won't interact with the other 50 binders,
but the bead numbered 1 won't interact with lamins and will interact with the other 50 binders with strength 1.

Run::

    stack exec -- motions --help

To get a detailed description of the arguments.

An example run would be::

    stack exec -- motions feat0.bed feat1.bed -l lengths -b binders -r 10 -x 10 -n 1000 -o out -s 100000 -i

The output of the simulation is given in the `PDB (Protein Data Bank) format`_.
How each energy vector, binder and chain is mapped to a string in the PDB format is described in a ".meta"
file created together with the output file.

.. _BED format: https://genome.ucsc.edu/FAQ/FAQformat.html#format1
.. _PDB (Protein Data Bank) format: http://www.wwpdb.org/documentation/file-format
