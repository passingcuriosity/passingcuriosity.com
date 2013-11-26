---
title: Installing a new ROM on my Nexus 7
tags: android, rom, nexus 7, OS X, sdk
excerpt: 
  I installed a new ROM on my Nexus 7 table today. This is what I did.
---

I use OS X with Homebrew to install additional softeare.

brew up
brew install android-sdk
export ANDROID_HOME=/usr/local/opt/android-sdk
echo "export ANDROID_HOME=/usr/local/opt/android-sdk" >> .profile

````{.bash}
android
````

The UI should automatically select the required packages for the current
version of Android. I'm going to use `adb` so I'll need the "Android SDK
Platform-tools" package.

Select this package (and any others you care about) and click install. A
license agreement window is opened. Agree to each license in turn, then click
the install button. 

Now the `adb` command should work.
