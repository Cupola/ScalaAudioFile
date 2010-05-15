/*
 * Created by IntelliJ IDEA.
 * User: rutz
 * Date: 15.05.2010
 * Time: 06:22:42
 */
package de.sciss.synth.io

import java.io.IOException
import java.nio.ByteOrder

trait AudioFileHeader {
   @throws( classOf[ IOException ])
   def readHeader( descr: AudioFileInfo ) : Unit
   @throws( classOf[ IOException ])
   def writeHeader( descr: AudioFileInfo ) : Unit
   @throws( classOf[ IOException ])
   def updateHeader( descr: AudioFileInfo ) : Unit
   def sampleDataOffset: Long
   def byteOrder: ByteOrder

   // WAV might overwrite this
   def isUnsignedPCM : Boolean = false

   // WAV and AIFF might overwrite this
   @throws( classOf[ IOException ])
   def readMarkers { /* empty */ }

   // AIFF might overwrite this
   @throws( classOf[ IOException ])
   def readAppCode { /* empty */ }

   @throws( classOf[ IOException ])
   protected final def readLittleUShort : Int = {
      val i = raf.readUnsignedShort()
      (i >> 8) | ((i & 0xFF) << 8)
   }

   @throws( classOf[ IOException ])
   protected final def readLittleInt : Int = {
      val i = raf.readInt()
      ((i>> 24) & 0xFF) | ((i >> 8) & 0xFF00) | ((i << 8) & 0xFF0000) | (i << 24)
   }

   @throws( classOf[ IOException ])
   protected final def readLittleLong : Long = {
      val n = raf.readLong()
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
   protected final def writeLittleShort( i: Int ) {
      raf.writeShort( (i >> 8) | ((i& 0xFF) << 8) )
   }

   @throws( classOf[ IOException ])
   protected final def writeLittleInt( i: Int ) {
      raf.writeInt( ((i >> 24) & 0xFF) | ((i >> 8) & 0xFF00) | ((i << 8)& 0xFF0000) | (i << 24) )
   }

   @throws( classOf[ IOException ])
   protected final def writeLittleLong( n: Long ) {
      raf.writeLong( ((n >> 56) & 0xFFL) |
                     ((n >> 40) & 0xFF00L) |
                     ((n >> 24) & 0xFF0000L) |
                     ((n >>  8) & 0xFF000000L) |
                     ((n <<  8) & 0xFF00000000L) |
                     ((n << 24) & 0xFF0000000000L) |
                     ((n << 40) & 0xFF000000000000L) |
                      (n << 56) )
   }

   @throws( classOf[ IOException ])
   protected final def readNullTermString : String = {
      val buf  = new StringBuffer()
      var b    = raf.readByte()
      while( b != 0 ) {
         buf.append( b.toChar )
         b	= raf.readByte()
      }
      buf.toString()
   }
}