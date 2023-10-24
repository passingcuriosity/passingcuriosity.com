---
title: Installing OpenBSD on a Microsoft Surface Go 3
tags: howto, openbsd, unix
excerpt: |
  Installing OpenBSD 7.2 on a Microsoft Surface Go 3 and doing some basic setup.
---

I have a new Microsoft Surface Go 3 and installed OpenBSD 7.2 to see how it
does as a machine to carry while travelling. The box contained the Surface Go
itself, a 24 W charger with the silly proprietary connector, and a few bits of
paper. I already had a Type Cover keyboard (from the Go I bought when it came
out) so I was pretty much ready to go. 

# Installing OpenBSD

0. Boot the Surface and let Microsoft Update install a bunch of stuff. Most of
   this isn't important as I blew away the Windows installation completely, but
   I guess the firmware updates will probably be useful (and there's no chance
   of getting them installed except through Windows). This takes absolutely
   ages. Something on the order of an hour, much of it with a static
   "installing updates" screen.

1. Download an OpenBSD installer image and write it to a USB storage device. I
   used `install72.img` and the `dd` command but it doesn't matter much. Don't
   forget to validate the signature of the image!

2. Boot the Surface into firmware mode (hold volume-up, press the power button,
   and wait until it enters the firmware before you release volume-up). Disable
   Secure Boot and set the boot order to include USB devices.

3. Insert the USB device and reboot. I needed a USB-C to USB-A converted for my
   USB storage device.

4. Go through the OpenBSD installer process. While OpenBSD includes a driver
   for the Intel wireless device, the installer doesn't include the firmware. I
   yanked out the USB storage device and inserted a USB Ethernet adaptor I had
   handy at the network configuration point. This allowed me to use an HTTP
   mirror as the source for the installation and, importantly, let installer
   download the appropriate firmware.


https://jcs.org/2020/05/15/surface_go2

# Networking

The installer identified the firmware packages needed for built-in Intel
wireless adaptor. But running `fw_update` is probably a good idea.

Configure the WiFi interface by creating `/etc/hostname.iwx0` with content like
the following:

```
join HOME_NETWORK wpakey PASSWORD
join WORK_NETWORK wpakey OTHERPASS
dhcp
```

There are more parameters that can be added; see `hostname.if(5)` and
`ifconfig(8)` for details.

I also have a new Google Pixel 7 Pro phone and want to get USB tethering up and
running. Recent Pixel models have stopped using RNDIS protocol for USB
tethering (though OpenBSD does have a driver for RNDIS). This is a good move by
Google! *And* they've moved to using an actual standard: CDC UNM. Unfortunately
OpenBSD does *not*, as far as I can tell, have a driver that supports CDC UNM
devices.

If your USB tethering device is supported, you can configure `hotplugd` to
bring up a network connection when you plug it in.

http://www.omarpolo.com/post/openbsd-tethering.html

# X, XenoDM, and a window manager

`xtsscale` to calibrate the touch screen but you need to find the right XInput
device. `xinput --list` will show a number of mouse devices (all alike except
for the device and XInput IDs).

I consulted `dmesg` for the `ims` devices:

```
dmesg | grep -E 'ims[0-9]'
```

On my machine `ims0` has "button, tip" while `ims1` also has "barrel, eraser"
listed: clearly `ims0` is the touch screen and `ims1` is the stylus support.
The other matched lines in `dmesg` should mention the `wsmouse` the corresponds
to each `ims` device: my machine had `wsmouse0` and `wsmouse1` respectively.
Use `xinput --list` and find the entry for `wsmouse0`. Then find the XInput ID
(e.g. `id=7`) and calibrate it:

```
xtsscale -d 7
```

If your fingers are as fat as mine the calibration process might take a few
attempts but eventually it will update the calibration parameters for the
running X server and print those same parameters so you can update your
configuration. The details are in `xtsscale(1)`.

https://www.birkey.co/2022-01-29-openbsd-7-xfce-desktop.html

https://www.tumfatig.net/2019/customizing-openbsd-xenodm/

https://dataswamp.org/~solene/2021-07-30-openbsd-xidle-xlock.html

https://www.tumfatig.net/2021/calibrate-your-touch-screen-on-openbsd/
