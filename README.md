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
 - a web browser (tested with Chrome version 37.0.2062.120 on linux and Chrome version 38.0.2125.102 on android)

Instalation
-----------
After cloning with

    git clone git@github.com:quintenpalmer/dungeons

From one terminal, run

    cd dungeons/server
    cabal install
    cabal configure
    cabal run server

Fron another terminal, run

    cd dungeons/web
    python server.py


From your web browser go to

[http://localhost:5000/dnd/4.0/character/](http://localhost:5000/dnd/4.0/character/)



Screenshot
----------
![Screen Shot](/Screenshot.png)
