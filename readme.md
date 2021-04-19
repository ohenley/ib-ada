# ib-ada
Interactive Brokers (IB) TWS/IB Gateway communication engine written in Ada.

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
- ib-ada is a 'semi-thick' TCP message-based request/response communication library (client) to the TWS or IB Gateway (server) written in the Ada programming language.

- I started this project because every other 'equivalent' implementation I tested did not work for my use case, for one reason or another, ... and I tried most of them all (C++, C#, Python, JS, official and third-party. I gave up at the idea of installing a whole Java developer environment just to test if, maybe, this time the outcome would be different).

## Status
- First. This library should work out of the box as I am using it daily. My experience is that it is debugged and quite robust as-is. 

- Supports only stocks (STK) related operations of USD currency. My needs. See [Limitations](#Limitations) section for more details. Expanding to different security types, currencies, etc. should be quite easy. At the moment I lack time, incentives and prefer to keep this project lean. I plan to add new features as they become needed.

- Supports only TWS/IB Gateway v978+ and server 152+ (latest as of April 2021).

- ib-ada adapts the IB TWS/IB Gateway 'mixed' communication model (request/response + 'request/subscription stream') to an orthodox request/response model for three main reasons. 
   1. To keep things synchronized as a first step/implementation.
   2. Filter out much of the scope noise. (check `build_place_order_msg (...)` inside `ib_ada-communication-outgoing.adb` for fun. Now, you should see the version trying to be backward compatible. e.g. [ib_insync](https://github.com/erdewit/ib_insync) does it.)
   3. Ease of use for client's code.
   
- This is a design decision that also presents some drawbacks because, like said before, TWS/IB Gateway shares its information using a simple message protocol but organized through different means (eg. open_orders are request/answer and profit_and_loss are request/subscription stream). Therefore some manipulations are done by ib-ada to 'close' everything as request/answer (eg. following a profit_and_loss request, a first received profit_and_loss data unit is automatically followed by an unsubscribe request to stop further incomming async profit_and_loss data units. This is transparent outside ib-ada and works very well actually) I am musing about implementing a fully threaded asynchronous message pump but such design opens a whole new can of worms. Ada would be the perfect fit though.

- Inside ib-ada, all IB far-west, distributed, most of the time undocumented string-based 'modelization' has been severely constrained to enums 'modeling'. Everything is strictly self-documented in `ib_ada.ads`. This helped lay out robust foundations from the get-go. Beware, some fundamental IB types equivalents are already present in ib_ada.ads but nothing supports them; they are not used anywhere. It is just a way to document and have them around for the future. 

- If by any bad luck, the ib-ada TCP message pump (call then read) gets stuck, it is really because of a yet still unencountered 'messaging context/sequence' coming from TWS/IB Gateway. TWS/IB Gateway reuses 'message types' for answers in different client call contexts, sometimes pushing irrelevant ones (eg. all your positions up front when you subscribe to a single symbol profit_and_loss data stream), in a new order, and potentially with different occurrences. Also, TWS/IB Gateway inconsistently make use of request ids (identifying client call and corresponding answer) and, like if it was not enough, often miss a high-level marker/message code identifying the complete end of a messages sequence for a particular client call. Do not get me wrong, it has such an 'end mechanism' for certain API calls but not all of them.    

- By chance, every answer context sequences (coming from TWS/IB Gateway) are, from observation, deterministic and unique so once we know what will come our way it is relatively easy to 'sandbox' the desired TCP read loop behaviour for this specific receiving context (see the combined use of `req_type/resp_type` throughout the different answer handlers in `ib_ada-communication-incomming.adb` for a better comprehension. It might not be the cleaner way but it works well. So for the moment thats that). You just get surprised the first time it bites you. Open an issue if/when that happens or help me out by proposing a pull request. I probably have not encountered all possible cases.

- This library is not, per se, properly sealed in terms of the highest theoretical standards. Also, some internal components are not reusable even if they could be candidates. Because I work on this through spare time, this is on purpose for now, to help move fast, refactor on the go as I 'reverse discovered' different implementation requirements, keep accessibility simple, and complexity down. I am aligned with the practice of 'semantic compression' as illustrated by Casey Muratori's mantra: [“make your code usable before you try to make it reusable”](https://caseymuratori.com/blog_0015).

## Prerequisites
- An activated Interactive Brokers (IB) account.
- Win32 or Linux platform (tested and working on Windows 10, Lubuntu 20.04.1)
- [TWS](https://www.interactivebrokers.ca/en/index.php?f=16040) or [IB Gateway](https://www.interactivebrokers.ca/en/index.php?f=16457)
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
- market data (warning: rudimentary and untested)

## Usage
- Being library code, ib-ada is meant to be driven by another application. See [ib-rest](https://github.com/ohenley/ib-rest).
- This library intended interface resides in the calls exposed by `ib_ada-communication.ads`. 
- You can test those:
```
$ cd tests
$ gprbuild tests.gpr
```

## Acknowledgments
- Thanks to @erdewit for his [ib_insync](https://github.com/erdewit/ib_insync) work which provided a sound reverse engineering 'map'.
- Thanks to @maxicus for his [ib-tws-api](https://github.com/maxicus/ib-tws-api) work which provided a sound reverse engineering 'map'.
