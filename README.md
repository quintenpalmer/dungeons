dungeons
========
Dungeons and Dragons 4.0 Character Sheet Automation


Motivation
----------
Automate all of the calculation required when leveling up and
increasing stats and present a character sheet for Dungeons
and Dragons 4.0 in a browser format.

Requirements
------------
 - ghc (tested with version 7.6.3)
 - python 2 (tested with version 2.7.6)
 - flask (tested with version 0.10.1)
 - ghc libraries:
    - base 4.6.*
    - containers 0.5.*
    - bytestring 0.10.*
    - aeson 0.6.* to 0.7.*
    - aeson-pretty 0.7.*
    - network 2.4.* to 2.5.*

Instalation
-----------
From one terminal, run

    cd ~/path/to/dungeons
    cabal install
    cabal configure
    cabal run server

Fron another terminal, run

    cd ~/path/to/dungeons/web
    python server.py


From your web browser go to

[http://localhost:5000/dnd/4.0/character/](http://localhost:5000/dnd/4.0/character/)



Screenshot
----------
![Screen Shot](/Screenshot.png)
