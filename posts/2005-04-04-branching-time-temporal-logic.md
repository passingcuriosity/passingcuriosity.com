---
title: Branching Time Temporal Logic
---
<a href="http://portal.acm.org/citation.cfm?id=800057.808661">Deciding branching time logic</a>

We've been looking at temporal logics for the last couple of lectures in Software Quality Management which has lead me to think a bit about the semantics of CTL (Computation-time Temporal Logic), primarily because they weren't spelt out in the lectures and the first page of results from <a href="http://scholar.google.com/">Google Scholar</a> didn't look promising.

CTL is a logic used to reason about the characteristics of programmes over time. As such it needs to be able to analyse a particular "run" (i.e. a linear sequence of states). It must also be able to deal with the non-determinism present in many programmes. To do this it must be able to handle branching of the successor relation, i.e. a state having multiple possible successors.

CTL has 4 modal operators: <strong>A</strong> ("for all paths"), <strong>E</strong> ("for some path"), <strong>F</strong> ("sometime") and <strong>G</strong> ("always"). These four operators are always used in pairs: <strong>AF</strong>, <strong>AG</strong>, <strong>EF</strong> and <strong>EG</strong>. The <strong>AG</strong> operator is equivalent to a &#9633; (the box or "necessary" operator) and the <strong>EF</strong> operator is equivalent to a &#9671; (the diamond or "possible" operator).

After a bit of thought, I think I've nearly got the semantics worked out in my head. At interpretation of CTL would be a Kripke frame (some worlds and a relation on them) and a valuation function (giving the truth values for formul&aelig; at worlds). The relation <span style="font-style: italic; font-weight: bolder">R</span> is irreflexive and non-transitive. The modal operators that handle the branching (<strong>A</strong> and <strong>E</strong>) are interpreted with <span style="font-style: italic; font-weight: bolder">R</span>. The linear-temporal modal operators (<strong>F</strong> and <strong>G</strong>) are interpreted with the reflexive and transitive closure of <span style="font-style: italic; font-weight: bolder">R</span>.

This is the point I have reached. Having written this, I don't think I've fully understood the implications of <strong>F</strong> and <strong>G</strong> being linear. I'll need to go through it again. On the plus, this might be useful for my thesis: all I need to do is try to find a natural-looking way to find the correct semantics for CTL, whatever they may be, using  counter-models for intended theorems of CTL. It is a fun exercise in any case.
