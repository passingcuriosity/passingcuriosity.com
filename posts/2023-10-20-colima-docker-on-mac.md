---
title: Use Colima to build and run container on macOS
tags: howto, linux, unix, docker, containers, macOS
excerpt: |
  Use Colima for Docker and Kubernetes development on your Mac.
---

If you use Homebrew to install third-party packages on macOS, it's
pretty straightforward to install and configure [Colima][1] along
with the various Docker and Kubernetes command-line tools.

[1]: https://github.com/abiosoft/colima

First, make sure your Homebrew formulas are all up to date:

```
brew update
```

Install Colima along with the Docker and Kubernetes command-line tools:
 
```
brew install colima docker kubectl
```

If you work on a network controlled by an organisation that uses TLS
stripping security appliances you'll probably need to install
additional CA root certificates before you can pull container images
from the Internet, etc.
You can put them in the usual place in your home directory and Colima
will automatically install them in the VMs it starts:

```
mkdir -p ~/.docker/certs.d
cd ~/.docker/certs.d
curl -o proxy-cert.crt https://insecurity.my.corp/proxy-cert.crt
```

(Do make sure you put each certificate in a separate file; if they are
concatenated you'll need to split them.)

With Colima installed, you should be able to start a Colima instance.
There are a handful of options to control the CPU, disk, and memory
allocation for the VM, the runtimes to configure on it, etc.
 
```
colima start --memory 8 --kubernetes
```
 
Check that the Docker and Kubernetes command-line tools have been
configured to talk to the new Colima instance:
 
```
kubectl get pods -A
docker ps
```

If you installed custom certs, you'd better check that it's all working
correctly by pulling an image:

```
docker pull python:3.12-slim
```
 
Easy.
