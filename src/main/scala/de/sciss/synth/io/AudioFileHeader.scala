/*
 *  AudioFileHeader.java
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

import java.nio.ByteOrder
import java.io.{ DataInputStream, DataOutputStream, IOException }

object AudioFileHeader {
   @throws( classOf[ IOException ])
   @inline protected final def readLittleUShort( dis: DataInputStream ) : Int = {
      val i = dis.readUnsignedShort()
      (i >> 8) | ((i & 0xFF) << 8)
   }

   @throws( classOf[ IOException ])
   @inline protected final def readLittleInt( dis: DataInputStream ) : Int = {
      val i = dis.readInt()
      ((i>> 24) & 0xFF) | ((i >> 8) & 0xFF00) | ((i << 8) & 0xFF0000) | (i << 24)
   }

   @throws( classOf[ IOException ])
   @inline protected final def readLittleLong( dis: DataInputStream ) : Long = {
      val n = dis.readLong()
      ((n >> 56) & 0xFFL) |
            ((n >> 40) & 0xFF00L) |
            ((n >> 24) & 0xFF0000L) |
            ((n >>  8) & 0xFF000000L) |
            ((n <<  8) & 0xFF00000000L) |
            ((n << 24) & 0xFF0000000000L) |
            ((n << 40) & 0xFF000000000000L) |
             (n << 56)
   }

   @throws( classOf[ IOException ])
   @inline protected final def writeLittleShort( dos: DataOutputStream, i: Int ) {
      dos.writeShort( (i >> 8) | ((i& 0xFF) << 8) )
   }

   @throws( classOf[ IOException ])
   @inline protected final def writeLittleInt( dos: DataOutputStream, i: Int ) {
      dos.writeInt( ((i >> 24) & 0xFF) | ((i >> 8) & 0xFF00) | ((i << 8)& 0xFF0000) | (i << 24) )
   }

   @throws( classOf[ IOException ])
   @inline protected final def writeLittleLong( dos: DataOutputStream, n: Long ) {
      dos.writeLong( ((n >> 56) & 0xFFL) |
                     ((n >> 40) & 0xFF00L) |
                     ((n >> 24) & 0xFF0000L) |
                     ((n >>  8) & 0xFF000000L) |
                     ((n <<  8) & 0xFF00000000L) |
                     ((n << 24) & 0xFF0000000000L) |
                     ((n << 40) & 0xFF000000000000L) |
                      (n << 56) )
   }

   @throws( classOf[ IOException ])
   protected final def readNullTermString( dis: DataInputStream ) : String = {
      val buf  = new StringBuffer()
      var b    = dis.readByte()
      while( b != 0 ) {
         buf.append( b.toChar )
         b	= dis.readByte()
      }
      buf.toString()
   }
}

trait AudioFileHeaderFactory {
   def createHeaderReader : Option[ AudioFileHeaderReader ]
   def createHeaderWriter : Option[ AudioFileHeaderWriter ]
   def identify( dis: DataInputStream ) : Boolean
}

trait AudioFileHeader {
//   def sampleDataOffset : Long
   def byteOrder : ByteOrder
   def spec : AudioFileSpec
//
//   /**
//    *    The number of frames in the file,
//    *    _according_ to the current header
//    *    information. This might not reflect
//    *    the actually written frames in
//    *    an output file, unless the header
//    *    is updated.
//    */
//   def numFrames : Long
//
//   @throws( classOf[ IOException ])
//   def seekFrame( frame: Long ) : Unit
}

//trait ReadableAudioFileHeader extends AudioFileHeader {
//   def createBufferReader( bufSize: Int ) : Option[ BufferReader ]
//}

trait AudioFileHeaderReader {
   /**
    *    Reads in the header information. Seek position
    *    should remain at the first frame of the audio data. 
    */
   @throws( classOf[ IOException ])
   def read( dis: DataInputStream ) : AudioFileHeader

   @inline protected def formatError = throw new IOException( "A header format error occurred" )

//   // WAV and AIFF might overwrite this
//   @throws( classOf[ IOException ])
//   def readMarkers { /* empty */ }
//
//   // AIFF might overwrite this
//   @throws( classOf[ IOException ])
//   def readAppCode { /* empty */ }
}

trait AudioFileHeaderWriter extends AudioFileHeader {
   @throws( classOf[ IOException ])
   def write( spec: AudioFileSpec ) : Unit
   @throws( classOf[ IOException ])
   def update( numFrames: Long ) : Unit

   def createBufferWriter : Option[ BufferWriter ]
   def createBufferHandler : Option[ BufferHandler ]
}