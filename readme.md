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
- ib-ada is a 'semi-thick' TCP message-based communication library (client) to the TWS or IB Gateway (server) written in the Ada programming language.

- I started this project because every other 'equivalent' implementation I tested did not work properly, for one reason or another, ... and I tried most of them all (C++, C#, Python, JS, official and third-party. I gave up before installing a whole Java developer environment just to test if, maybe, this time the outcome would be different).

- I hold a certain grudge against Interactive Brokers for misleading advertising about their technology offering. I paid the price of such a painful discovery through hundred hours of research, testing, dead-end, and headaches. This library is a conservative, explicit, sandboxed, progressive, simple, and hopefully sane response to this reality. 

- From an 'archeological/reverse engineering' point of view we can argue that Interactive Brokers relevance and churn is subsided by technical debt; probably on purpose as a way to pressure their premium product adoption (FIX protocol access @ 1500 USD/month)

- Looking at the retail securities investment landscape, my view is that there is a massive opportunity for a robust, well-designed programmatic retail investment infrastructure.

- For the time being, as a Canadian, my only API choice without resorting to opening a US corporation is to use Interactive Brokers.

## Status
- First. This library should work out of the box as I am using it daily. My experience is that it is debugged and quite robust as-is. I take the time to mention it because if you did not know yet, few open source pet projects repository actually builds/works. You can dig a lot of time trying John Doe(s) projects to realize you are still left cold. I am just trying to save you time, something nobody did for me while getting acquainted with the 'IB retail stack'.

- Supports only stocks (STK) related operations of USD and CAD currencies. My needs. See [Limitations](#Limitations) section for more details. Expanding to different security types, currencies, etc. should be quite easy. At the moment I lack time, incentives and prefer to keep this project lean. I plan to add new features as they become needed. Why? Because the full IB scope is, in my opinion, dubious. 

- Supports only TWS/IB Gateway v978+ and server 152+.

- This library is not, per se, properly sealed in terms of the highest theoretical standards. Some internal components are not reusable even if they could be candidates. This is on purpose, for now, to help move fast, refactor on the go as I 'reverse discovered' different implementation requirements, keep accessibility simple, and complexity down. I am aligned with the practice of 'semantic compression' as illustrated by Casey Muratori's mantra: [“make your code usable before you try to make it reusable”](https://caseymuratori.com/blog_0015).

- All IB 'far-west, distributed and undocumented' string-based modelization has been severely constrained to enums modeling. Everything is strictly self-documented in ib_ada.ads. This helped lay out robust foundations from the get-go. Beware, some IB types equivalents are already present in ib-ada but nothing supports them; they are not used anywhere. It is just a way to document and have them around for the future. 

- This version of ib-ada adapts the IB TWS/IB Gateway 'mixed' communication model ('client-server' + 'streams' + 'message semantic context dependent') to an orthodox client-server model for two main reasons. 
   1. To keep things synchronized as a first step/implementation.
   2. Filter out much of the scope noise. (check build_place_order_msg (...) inside ib_ada-communication-outgoing.adb for fun. Now, you should see the version trying to be backward compatible. e.g. [ib_insync](https://github.com/erdewit/ib_insync) does it.)
  This is a design decision that also presents some drawbacks because TWS/IB Gateway went at length to break this communication model. I am musing about implementing a fully threaded asynchronous message pump but such design opens a whole new can of worms. 

- If by any bad luck, the ib-ada TCP message pump (call then read) gets stuck, it is really because of a yet still unencountered 'messaging context/sequence' coming from TWS/IB Gateway. TWS/IB Gateway reuse 'message types' for answers in different client call contexts, sometimes pushing irrelevant ones, in a new order, and potentially with different occurrences. Also, TWS/IB Gateway is inconsistent in its make use of request ids (identifying client call and corresponding answer) and, like if it was not enough, often miss a high-level marker/message code identifying the complete end of a messages sequence for a particular client call. Do not get me wrong, it has such an 'end mechanism' for certain API calls but not all of them; because it also thinks it was a good idea to simultaneously be a streaming server on the same port and finally confused one responsibility to the other in modeling an API of clear intents.    

- By chance, every answer context sequences (coming from TWS/IB Gateway) are, from observation, deterministic and unique so once we know what will come our way it is easy to 'encode' the receiving context behavior (see the use of req_type/resp_type used in combination throughout the different answer handlers. It might not be the cleaner way but it works well. So for the moment thats that). You just get surprised the first time. Open an issue if/when that happens or help me out by proposing a pull request.

## Prerequisites
- An activated Interactive Brokers (IB) account.
- [TWS](https://www.interactivebrokers.ca/en/index.php?f=16040) or [IB Gateway](https://www.interactivebrokers.ca/en/index.php?f=16457) running and serving to standard IB ports. (or consult ib_ada.ads and adapt preconfigured standard ones) 
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
- market data (warning: rudimentary and untested)

## Usage
Being library code, ib-ada is to be driven by another application. See [ib-rest](https://github.com/ohenley/ib-rest).
This library intended interface resides in the calls exposed by ib_ada-communication.ads. 

## Acknowledgments
- Thanks to @erdewit for his [ib_insync](https://github.com/erdewit/ib_insync) work which provided a sound 'reverse engineering map'.
- Thanks to @maxicus for his [ib-tws-api](https://github.com/maxicus/ib-tws-api) work which provided a sound 'reverse engineering map'.
