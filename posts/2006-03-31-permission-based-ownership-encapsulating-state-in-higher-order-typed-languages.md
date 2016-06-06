---
wordpressid: 1664
layout: post
title: "Permission-based ownership: encapsulating state in higher-order typed languages"
tags: papers, reading, programming languages
wordpressurl: http://passingcuriosity.com/2006/permission-based-ownership-encapsulating-state-in-higher-order-typed-languages/
---

[Permission-based ownership: encapsulating state in higher-order typed
languages][1] by Neel Krishnaswami and Jonathan Aldrich. In PLDI'05.

I'm still puzzling through the examples, but it looks pretty cool. The
essential idea is to use the type system (an extension of <acronym>System
F</acronym> with references and ownership called <acronym>System
F<sub><i>own</i></sub></acronym>) to ensure that the internal details of
modules cannot be messed with.

They give an illustrative example involving customers in one domain, banking
machinery in a second and account details in a third. The customers are allowed
to call the banking machinery, and the banking machinery can access the account
details, and all other access is invalid. The goal of <acronym>System
F<sub><i>own</i></sub></acronym> is to prove these sorts of properties.

- <a href="http://portal.acm.org/citation.cfm?id=1065023">ACM</a>
- <a href="http://scholar.google.com/scholar?hl=en&lr=&safe=off&cluster=17777635342074588239">Google</a>
- <a href="http://www.citeulike.org/article/471760">CiteULike</a>

[1]: http://www.cs.cmu.edu/~aldrich/papers/pldi05.pdf
