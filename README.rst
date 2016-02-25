=======
Motions
=======

Motions is a MCMC simulation of chromatin movements within a cell nucleus
using a variation of the "Strings and Binders Switch" model.

Installation
------------

`Stack`_ is the easiest way to build and run the simulation.
First you need to install `stack`_. Installation depends on the platform.
Most linux distributions should provide an easy way to get `stack`_ from
repositories. Example installation on Debian 8 x64::

    sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 575159689BEFB442
    echo 'deb http://download.fpcomplete.com/debian jessie main'|sudo tee /etc/apt/sources.list.d/fpco.list
    sudo apt-get update && sudo apt-get install stack -y

Detailed instructions can be found in `stack's documentation`_.

When you have stack you can proceed with building the simulation.
The following commands do everything for you::

    git clone https://github.com/Motions/motions.git
    cd motions
    stack setup
    stack build

Some dependencies require a fair amount of RAM when being built. Our experiences
tell that at least 3 GB is necessary (use swap if needed).

.. _stack: http://docs.haskellstack.org/en/stable/README.html
.. _stack's documentation: http://docs.haskellstack.org/en/stable/README.html#how-to-install

Running the simulation
----------------------

You can either use stack (if you used stack build)::

    stack exec -- motions

Or simply execute it from the directory containing the executable (the path may vary a little)::

    cd .stack-work/dist/x86_64-linux/Cabal-1.22.4.0/build/motions/
    ./motions

Motions supports simulation of multiple chains. Chain beads have associated with them energy vectors
that describe how chains interact with various binders floating inside the cell nucleus.
The descriptions of chains are given using:

    1. Files in the `BED format`__ to describe the different chain features
           that will later be translated to energy vectors.
    2. One file containing lengths of chains given by a line of integers
           separated with spaces.
    3. The simulation resolution given by a command line argument.

Example input containing chain descriptions could be:

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
0-9, 20-29 (twice, which means stronger feature), 40-49, 80-89. The second
feature (described in the "feat1.bed" file) appears on the following ranges: 10-19, 30-39.

The resolution is used to determine how these base pair chains translate to bead chains
and their energy vectors. The resolution describes how ranges of base pairs are mapped
to beads, for example if the resolution equals 10, that means that the ranges 0-9, 10-19...
are matched to beads 0, 1... The strenght of a feature in a bead is equal to the number
of feature appearances in the corresponding range.

In the example there are going to be 10 beads. The energy vectors are of length 2 because
there are 2 features. The entries in these vectors are the corresponding feature strenghts.
The resulting vectors are::

    [1,0], [0,1], [2,0], [0,1], [1,0], [0,0], [0,0], [0,0], [1,0], [0,0]

The cell nucleus contains floating binders which interact with chains. Energy vectors
are used to determine how each bead interacts with binders of different types.
The number of different binder types is equal to the length of an energy vector,
which in turn is equal to the number of chain features (number of BED files).
There always appears at least one type of binder - the lamin. That means there always
has to be at least one BED file even if it's empty (which means no bead interacts with lamins).

The numbers of binders of each type (excluding lamins) are given using a file.
Example file "binders"::

    50

describes that there are 50 binders whose types correspond to the second entry of an energy vector
(the first entry corresponds to lamins). Following the example, the bead numbered 2 will interact
with lamins with strength 2 and won't interact with the other 50 binders, but the bead numbered 1
won't interact with lamins and will interact with the other 50 binders with strength 1.

When executing the simulation, the BED file names are given using positional arguments, the chain lengths file
using the (-l) option, the binder counts using the (-b) option, the resolution using the (-x) option.
The other required arguments are: the radius of the cell (-r), the number of state initialisation attempts (-n),
the output file name (-o) and the number of simulation steps (-s).
There is an optional switch whether to write intermediate PDB frames (-i). If it's off (default) only the last
frame is written.

You can also choose the score function which directs the simulation (--score). There is only one score function
currently available, "StandardScore".
At last, you can choose how the simulation represents its state internally (--representation). The possible
values are "PureChain" (default) and "IOChain".

The output is given in the `PDB (Protein Data Bank) format`__.

Run::

    stack exec -- motions --help

To get a detailed description of the arguments.

An example run would be::

    stack exec -- motions feat0.bed feat1.bed -l lengths -b binders -r 10 -x 10 -n 1000 -o out -s 100000 -i

.. _BED format: https://genome.ucsc.edu/FAQ/FAQformat.html#format1
.. _PDB (Protein Data Bank) format: http://www.wwpdb.org/documentation/file-format
