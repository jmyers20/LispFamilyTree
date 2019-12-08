# LispFamilyTree
Family Tree Project using Lisp

PATH:
"C:\Users\jmyer\Desktop\github\LispFamilyTree\CSC1800TestCases\TestInputs\test2.txt"

change above path to:
"C:\\\Users\\\jmyer\\\Desktop\\\github\\\LispFamilyTree\\\CSC1800TestCases\\\TestInputs\\\test2.txt"

To EXECUTE family.lisp, run this in the Listerner:
(LET ((str (OPEN "C:\\\Users\\\jmyer\\\Desktop\\\github\\\LispFamilyTree\\\CSC1800TestCases\\\TestInputs\\\test2.txt")))
	(family str))

## Project Background
This project deals with biological family trees. A family tree is defined in this project as a set of
individuals who are “connected” to each other by genetic relationships. This program answers questions about different specializations of “related”
between people in a family tree. There are two kinds of questions it handles:

A) “IS-A” Questions: These are “Yes/No” questions about whether two
people <A> and <B> share the following relationships:
1. Is <A> a child of <B> (order of names matters!)
2. Is <A> a sibling of <B>
3. Is <A> an ancestor of <B>
4. Is <A> a cousin of <B>
5. Is <A> unrelated to <B>

B) “WHO-IS-A” Questions: “Who are all the people who are A’s
<relation>?” <Relation> can be any of the relationships listed for “IS-A”
Questions.

## Input Format
Each line in the family tree will have the form
E <name1> <name2> or E <name1> <name2> <name3>
which has the meaning “<name1> and <name2> are married” or “the married parents <name1>
and <name2> produced a child <name3>.”

## Queries
QUERY Meaning
X <name1> <relation> <name2> - Is <name1> the <relation> of <name2>?
W <relation> <name1> 				 - List everyone who is the <relation> of <name1>.
<relation> can be child, sibling, ancestor, cousin, or unrelated.
