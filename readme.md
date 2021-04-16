# ib-ada
Interactive Brokers (IB) TWS/IB Gateway communication engine written in Ada.

> The bitterness of poor quality remains long after the sweetness of low price is forgotten.
>
> -- <cite>Benjamin Franklin</cite>

## Table of Contents
<details>
<summary>Click to expand</summary>

1. [About](#About)
2. [Status](#Status)
3. [Prerequisites](#Prerequisites)  
4. [Dependencies](#Dependencies)
5. [Building](#Building)
   1. [Windows](#Windows)
   2. [Other OSes](#Other-OSes)
6. [Installation](#Installation)
7. [Limitations](#Limitations)
8. [Usage](#Usage)
9. [Acknowledgments](#Acknowledgments)

</details>

## About
- I started this project because every other 'equivalent' implementation I tested, somewhat, did not work properly ... and I tried most of them all (C++, C#, Python, JS, official and third-party. I gave up before installing a whole Java developer environment just to test if, maybe, this time the outcome would be different).

- I hold a certain grudge against Interactive Brokers for misleading advertising about their technology offering. I paid the price of such a painful discovery through hundred hours of research, testing, dead-end, and headaches. This library is a conservative, explicit, sandboxed, progressive, simple, and hopefully sane response to this reality. 

- From an 'archeological/reverse engineering' point of view we can argue that Interactive Brokers relevance and churn is subsided by technological debt; probably on purpose as a way to pressure their premium product adoption (FIX protocol access @ 1500 USD/month)

- Looking at the retail securities investment landscape, my view is that there is a massive opportunity for a robust, well-designed programmatic retail investment infrastructure.

## Status
- This library is not, per se, properly sealed in terms of the highest theoretical standards. Some internal components are not reusable even if they could be candidates. This is on purpose, for now, to help move fast, refactor on the go as I reverse discovered what implementation requirements emerged, keep accessibility simple, complexity down. I am aligned with semantic compression as Casey Muratori illustrates using this mantra: [“make your code usable before you try to make it reusable”](https://caseymuratori.com/blog_0015).

- All IB 'far-west, distributed and undocumented' string-based modelization have been severely constrained to enums modeling. Everything is self-documented in ib_ada.ads. This helped layout robust foundations from the get-go, which can now confidently scale up (if you do not find the type variant you need, add it to the proper enum, it will propagate everywhere and we are done. No ambiguity, ever, at compile time)

- WIP

## Prerequisites
- Win32 or Linux platform (tested and working on Windows 10, Lubuntu 20.04.1)
- GNAT (tested and working with GNAT community 2020, GNAT FSF 9.3.0)

## Dependencies
- none

## Building
#### Windows
- Install [GNAT community 2020](https://community.download.adacore.com/v1/966801764ae160828c97d2c33000e9feb08d4cce?filename=gnat-2020-20200429-x86_64-windows-bin.exe)
```
$ git clone https://github.com/ohenley/ib-ada.git    
$ cd ib-ada
$ gprbuild ib_ada.gpr
```
   
#### Linux (ubuntu 20.04.1+ flavors)
```
$ sudo apt-get install gnat-gps
$ git clone https://github.com/ohenley/ib-ada.git
$ cd ib-ada
$ gprbuild ib_ada.gpr
```

## Installation
Not Applicable.

## Limitations
Only works for stocks and provides a minimum viable interface to the TWS/IB Gateway for a typical trading bot. Complete but no fancy, namely:

- accounts information (account ids and different balance types)
- positions
- profit and loss for positions
- open orders
- place orders
- potential order commission (through fake place orders)

## Usage
Being library code, ib-ada is to be driven by another application. See [ib-rest](https://github.com/ohenley/ib-rest).
This library interface is the calls exposed in ib_ada-communication.ads/adb. 

## Acknowledgments
- Thanks to @erdewit for his [ib_insync](https://github.com/erdewit/ib_insync) work which provided a sound 'reverse engineering map'.
- Thanks to @maxicus for his [ib-tws-api](https://github.com/maxicus/ib-tws-api) work which provided a sound 'reverse engineering map'.
