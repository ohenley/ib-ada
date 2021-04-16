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
- I started this project because every other 'equivalent' implementation I tested, somewhat, did not work properly ... and I tried most of them all (C++, C#, Python, JS, official and third-party. I gave up before installing a whole Java developper environment just to test if, maybe, this time the outcome would be different).

- I hold a firm grudge against Interactive Brokers for misleading advertising about their technology offering. I personnally paid the price of such painfull discovery through hundred hours of research, testing, dead end and headaches. This library is a conservative, explicit, sandboxed, progressive, simple, and hopefully sane response to this reality. 

- From an 'archeological/reverse engineering' point of view we can argue that Interactive Brokers relevance and churn is subsided by technological debt; probably on purpose as a way to pressure their premium product adoption (FIX protocol access @ 1500 USD/month)

- Looking at the retail securities investment landscape, my view is that there is massive opportunity for a robust, well designed programmatic retail investment infrastructure.

## Status
- WIP text. :)

## Prerequisites
- Win32 or Linux platform (tested and working on Windows 10, Lubuntu 20.04.1)
- GNAT (tested and working with GNAT community 2020, GNAT FSF 9.3.0)

## Dependencies
- none

## Building
#### Windows
- Install (GNAT community 2020)[https://community.download.adacore.com/v1/966801764ae160828c97d2c33000e9feb08d4cce?filename=gnat-2020-20200429-x86_64-windows-bin.exe]
`$ git clone https://github.com/ohenley/ib-ada.git`      
`$ cd ib_ada`
`$ gprbuild ib_ada.gpr`
   
#### Linux (ubuntu 20.04.1+ flavors)
`$ sudo apt-get install gnat-gps`
`$ git clone https://github.com/ohenley/ib-ada.git`
`$ cd ib_ada`
`$ gprbuild ib_ada.gpr`

## Installation
Not Applicable.

## Limitations
Only works for stocks and provides a minimum viable interface to the TWS/IB Gateway for a typical trading bot. Complete but no fancy, namely:

- accounts informations (account_ids and different balance types)
- positions
- profit and loss for positions
- open orders
- place orders
- potential order commission (through fake place orders)

## Usage
Being library code, ib-ada is to be driven by another application. See (ib-rest)[https://github.com/ohenley/ib-rest].
This library interface are the calls exposed in ib_ada-communication.ads/adb. 

## Acknowledgments
- Thanks to @erdewit for his (ib_insync)[https://github.com/erdewit/ib_insync] work which provided a sound 'reverse engineering map'.
- Thanks to @maxicus for his (ib-tws-api)[https://github.com/maxicus/ib-tws-api] work which provided a sound 'reverse engineering map'.