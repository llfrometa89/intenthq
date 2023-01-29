package com.intenthq.challenge

import scala.annotation.tailrec

object SNiceStrings {

// From http://adventofcode.com/day/5
//  --- Day 5: Doesn't He Have Intern-Elves For This? ---
//
//  Santa needs help figuring out which strings in his text file are naughty or nice.
//
//    A nice string is one with all of the following properties:
//
//    It contains at least three vowels (aeiou only), like aei, xazegov, or aeiouaeiouaeiou.
//  It contains at least one letter that appears twice in a row, like xx, abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
//    It does not contain the strings ab, cd, pq, or xy, even if they are part of one of the other requirements.
//    For example:
//
//    ugknbfddgicrmopn is nice because it has at least three vowels (u...i...o...), a double letter (...dd...), and none of the disallowed substrings.
//  aaa is nice because it has at least three vowels and a double letter, even though the letters used by different rules overlap.
//    jchzalrnumimnmhp is naughty because it has no double letter.
//    haegwjzuvuyypxyu is naughty because it contains the string xy.
//    dvszwmarrgswjxmb is naughty because it contains only one vowel.
//    How many strings are nice?

  final private val Vowels              = "aeiou"
  final private val ForbiddenSubStrings = List("ab", "cd", "pq", "xy")

  def nice(xs: List[String]): Int = xs.count(isNice)

  private def isNice(text: String): Boolean =
    hasAtLeastThreeVowels(text) && hasLettersAppearingTwiceInRow(text) && !hasForbiddenSubString(text)

  private def isVowel(letter: Char): Boolean = Vowels.contains(letter.toLower)

  private def hasAtLeastThreeVowels(text: String): Boolean = text.map(isVowel).count(v => v) >= 3

  private def isEqualsToPreviousChar(letter: Char, previousChar: Char): Boolean = letter == previousChar

  private def hasLettersAppearingTwiceInRow(text: String): Boolean = {

    @tailrec
    def finder(text: List[Char], previousCharacter: Char): Boolean =
      text match {
        case Nil => false
        case firstLetter :: letters =>
          if (isEqualsToPreviousChar(firstLetter, previousCharacter)) true
          else finder(letters, firstLetter)
      }

    finder(text.toList, Char.MinValue)
  }

  private def hasForbiddenSubString(text: String): Boolean = ForbiddenSubStrings.exists(sub => text.contains(sub))
}
