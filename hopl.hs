#!/usr/bin/env runhaskell

-- Andrew Pennebaker
-- andrew.pennebaker@gmail.com
-- 19 Aug 2011
--
-- Data from "Timeline of programming languages"
-- http://en.wikipedia.org/wiki/Timeline_of_programming_languages
--
-- Inspired by O'Reilly's "The History of Programming Languages"
-- http://oreilly.com/news/languageposter_0504.html
--
-- Requirements:
--
-- Haskell 2011 (http://haskell.org/)
-- Haskell graphviz (http://hackage.haskell.org/packages/archive/graphviz/latest/doc/html/Data-GraphViz.html)
-- Graphviz (http://www.graphviz.org/)

module Hopl where

import Data.Graph.Inductive
import Data.GraphViz
import Maybe (fromJust)

data Lang = Lang {
		name :: String,
		parents :: [String]
	}

langs2Graph :: Gr String String -> [Lang] -> Gr String String
langs2Graph g [] = g
langs2Graph g (l:ls) = langs2Graph g'' ls
	where
		n' = (head $ newNodes 1 g, name l)
		g' = insNode n' g
		g'' = langs2Graph' g' n' (parents l)

langs2Graph' :: Gr String String -> LNode String -> [String] -> Gr String String
langs2Graph' g n' [] = g
langs2Graph' g n' (p:ps) = langs2Graph' g' n' ps
	where
		ns = nodes g
		n = head $ filter ((p ==) . fromJust . lab g) ns
		g' = insEdge (n, fst n', "") g

langs :: [Lang]
langs = [
		Lang "Pythagoras" [],
		Lang "Music" ["Pythagoras"],
		Lang "Music boxes" ["Music"],
		Lang "Coockoo clocks" ["Music boxes"],
		Lang "Jacquard loom punch cards" ["Coockoo clocks"],
		Lang "Player pianos" ["Coockoo clocks"],
		Lang "Analytical Engine order code" ["Jacquard loom punch cards"],
		Lang "Plankalkül" ["Analytical Engine order code"],
		Lang "Number theory" ["Pythagoras"],
		Lang "Turing machines" ["Analytical Engine order code", "Number theory"],
		Lang "Cams" ["Player pianos"],
		Lang "Numerical control languages" ["Cams"],
		Lang "Transistors" [],
		Lang "Integrated circuits" ["Transistors"],
		Lang "von Neumann architecture" ["Integrated circuits", "Turing machines"],
		Lang "Set theory" ["Number theory"],
		Lang "Game of Life" ["Turing machines"],
		Lang "Assembly" ["von Neumann architecture"],
		Lang "P′′" ["Turing machines"],
		Lang "Brainfuck" ["P′′"],
		Lang "Ook!" ["Brainfuck"],
		Lang "Application-specific integrated circuits" ["Integrated circuits"],
		Lang "Hardware description languages" ["Application-specific integrated circuits"],
		Lang "Very High Speed Integration Circuits" ["Hardware description languages"],
		Lang "ENIAC Short Code" ["Assembly"],
		Lang "Brief Code" ["ENIAC Short Code"],
		Lang "Short Code" ["Brief Code"],
		Lang "CISC" ["Short Code"],
		Lang "RISC" ["Short Code"],
		Lang "ARC" ["RISC"],
		Lang "ARM" ["RISC"],
		Lang "DEC Alpha" ["RISC"],
		Lang "29k" ["RISC"],
		Lang "Apple PowerPC" ["RISC"],
		Lang "SPARC" ["RISC"],
		Lang "Redcode" ["CISC"],
		Lang "x86" ["CISC"],
		Lang "MASM" ["x86"],
		Lang "TASM" ["MASM"],
		Lang "FASM" ["x86"],
		Lang "NASM" ["x86"],
		Lang "GNU Gas" ["x86"],
		Lang "x86-64" ["x86"],
		Lang "FORTRAN" ["von Neumann architecture"],
		Lang "SNOBOL" ["FORTRAN"],
		Lang "gfortran" ["FORTRAN"],
		Lang "ALGOL" ["FORTRAN"],
		Lang "Pascal" ["ALGOL"],
		Lang "Ada" ["Pascal"],
		Lang "VHDL" ["Very High Speed Integration Circuits", "Ada"],
		Lang "Icon" ["ALGOL"],
		Lang "SAIL" ["ALGOL"],
		Lang "TeX" ["SAIL"],
		Lang "LaTeX" ["TeX"],
		Lang "A" ["von Neumann architecture"],
		Lang "ARITH-MATIC" ["A"],
		Lang "MATH-MATIC" ["ARITH-MATIC"],
		Lang "FLOW-MATIC" ["MATH-MATIC"],
		Lang "COMTRAN" ["FLOW-MATIC"],
		Lang "Controlled natural languages" [],
		Lang "Basic English international auxiliary language" ["Controlled natural languages"],
		Lang "FACT" ["Basic English international auxiliary language"],
		Lang "COBOL" ["COMTRAN", "FACT"],
		Lang "PL/I" ["ALGOL", "COBOL"],
		Lang "Multics PL/I" ["PL/I"],
		Lang "MIT Multics" ["Multics PL/I"],
		Lang "Thompson shell" ["MIT Multics"],
		Lang "Bourne shell" ["Thompson shell"],
		Lang "csh" ["Bourne shell"],
		Lang "tcsh" ["csh"],
		Lang "Bash" ["Bourne shell"],
		Lang "Incompatible Timesharing System" ["MIT Multics"],
		Lang "Tape Editor and Corrector" ["Incompatible Timesharing System"],
		Lang "E" ["Tape Editor and Corrector"],
		Lang "GNU Emacs" ["E"],
		Lang "G-code" ["Numerical control languages"],
		Lang "G" ["G-code"],
		Lang "MATLAB" ["FORTRAN"],
		Lang "Octave" ["MATLAB"],
		Lang "MathScript" ["G"],
		Lang "LabVIEW" ["MathScript"],
		Lang "AgentSheets" ["LabVIEW"],
		Lang "LEGO Mindstorms" ["AgentSheets"],
		Lang "LEGOsheets" ["LEGO Mindstorms"],
		Lang "CPL" ["ALGOL"],
		Lang "BCPL" ["CPL"],
		Lang "B" ["BCPL"],
		Lang "C" ["B"],
		Lang "NQC" ["LEGO Mindstorms", "C"],
		Lang "qed" [],
		Lang "ed" ["qed", "BCPL"],
		Lang "sed" ["ed"],
		Lang "GNU grep" ["sed"],
		Lang "SETL" ["Set theory"],
		Lang "ABC" ["B", "SETL"],
		Lang "Simula" ["ALGOL"],
		Lang "C++" ["C"]
	]

langGraph :: Gr String String
langGraph = langs2Graph empty langs

langParams :: GraphvizParams String String () String
langParams = nonClusteredParams {
		globalAttributes = ga,
		fmtNode = fn,
		fmtEdge = fe
	}
	where
		ga = [
			GraphAttrs [
				(BgColor . X11Color) Transparent
				],
			NodeAttrs [
				Shape PlainText,
				(FillColor . X11Color) Transparent,
				Style [SItem Filled []]
				]
			]

		fn (n,l) = [(Label . StrLabel) l]
		fe (f,t,l) = [(Label . StrLabel) l]

main :: IO ()
main = putStr $ printDotGraph $ graphToDot langParams langGraph