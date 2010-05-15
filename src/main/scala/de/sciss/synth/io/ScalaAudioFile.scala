package de.sciss.synth.io

object ScalaAudioFile {
   val name          = "ScalaAudioFile"
   val version       = 0.10
   val copyright     = "(C)opyright 2004-2010 Hanns Holger Rutz"

   def versionString = (version + 0.001).toString.substring( 0, 4 )

   def main( args: Array[ String ]) {
      printInfo
      System.exit( 1 )
   }

   def printInfo {
      println( "\n" + name + " v" + versionString + "\n" + copyright +
         ". All rights reserved.\n\nThis is a library which cannot be executed directly.\n" )
   }
}