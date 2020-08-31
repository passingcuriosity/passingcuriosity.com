---
title: iiNet NBN VDSL2 with Proscend VSDL2 SFP module
location: Sydney, New South Wales
tags: howto, vdsl, nbn, iinet, edgerouter, proscend, spf
excerpt:
  Connecting my home network to a new iiNet NBN services with a Proscend T-180 VSDL2 SFP module.
---

My apartment building just got an [NBN][1] [FTTB][2] node installed and I got
my connection jumpered on Tuesday morning. This is a short rundown of the
configuration so that I'll be able to find all the details late.

The hardware involved is:

1. A [Proscend T-180 VDSL2 SFP][3] module.

2. A [Ubiquiti EdgeRouter X SFP][4] router.

The installation goes like so:

1. Install the SFP module into the router, making sure that the little clip
   engages to help keep the module formly seated.

2. Connect the cable between the wall plate and the module. I used RJ-45 CAT-5e
   because that's what I had on hand.

3. Watch the sync light on the module blink and then turn on when you are
   connected. Success!

4. Spend a lot of time on your mobile phone searching for the correct
   parameters to connect to your ISP. In the process, discover that iiNet has
   at least two different lists of parameters for NBN FTTP VSDL connection
   details.

5. Enable the SFP interface (if it isn't already enabled) and add a VLAN
   interface with VLAN ID 2.

6. Add a PPPoE interface on the VLAN interface, use your iiNet authentication
   details.

7. Wait and you should be allocated an IP address on the PPPoE interface.

Now your router is connected! In my case, I had additional details for
configure -- firewall, masquerading, etc. -- but that's pretty much it.

[1]: https://www.nbnco.com.au/
[2]: https://www.nbnco.com.au/learn/network-technology/fibre-to-the-building-explained-fttb
[3]: https://www.proscend.com/en/product/VDSL2-SFP-Modem-for-Telco/180-T.html
[4]: https://www.ui.com/edgemax/edgerouter-x-sfp/
