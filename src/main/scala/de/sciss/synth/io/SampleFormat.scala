/*
 *  SampleFormat.scala
 *  (ScalaAudioFile)
 *
 *  Copyright (c) 2004-2010 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *	 For further information, please contact Hanns Holger Rutz at
 *	 contact@sciss.de
 *
 *
 *  Changelog:
 */

package de.sciss.synth.io

abstract class SampleFormat( val id: String, val bitsPerSample: Int ) {
   def readerFactory  : Option[ BufferReaderFactory ]
   def writerFactory  : Option[ BufferWriterFactory ]
   def bidiFactory    : Option[ BufferBidiFactory ]
}

object SampleFormat {
   case object UInt8 extends SampleFormat( "uint8", 8 ) {
      def readerFactory = Some( BufferReader.Byte )
      def writerFactory = Some( BufferWriter.Byte )
      def bidiFactory   = Some( BufferBidi.Byte )
   }
   case object Int8 extends SampleFormat( "int8", 8 ) {
      def readerFactory = Some( BufferReader.Byte )
      def writerFactory = Some( BufferWriter.Byte )
      def bidiFactory   = Some( BufferBidi.Byte )
   }
   case object Int16 extends SampleFormat( "int16", 16 ) {
      def readerFactory = Some( BufferReader.Short )
      def writerFactory = Some( BufferWriter.Short )
      def bidiFactory   = Some( BufferBidi.Short )
   }
   case object Int24 extends SampleFormat( "int24", 24 ) {
      def readerFactory = Some( BufferReader.ThreeBytes )
      def writerFactory = Some( BufferWriter.ThreeBytes )
      def bidiFactory   = Some( BufferBidi.ThreeBytes )
   }
   case object Int32 extends SampleFormat( "int32", 32 ) {
      def readerFactory = Some( BufferReader.Int )
      def writerFactory = Some( BufferWriter.Int )
      def bidiFactory   = Some( BufferBidi.Int )
   }
   case object Float extends SampleFormat( "float", 32 ) {
      def readerFactory = Some( BufferReader.Float )
      def writerFactory = Some( BufferWriter.Float )
      def bidiFactory   = Some( BufferBidi.Float )
   }
   case object Double extends SampleFormat( "double", 64 ) {
      def readerFactory = Some( BufferReader.Double )
      def writerFactory = Some( BufferWriter.Double )
      def bidiFactory   = Some( BufferBidi.Double )
   }
// case object MuLaw  extends SampleFormat( "mulaw" )
// case object ALaw   extends SampleFormat( "alaw" )
}