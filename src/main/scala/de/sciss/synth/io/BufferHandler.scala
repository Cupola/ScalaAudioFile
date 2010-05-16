/*
 *  BufferHandler.scala
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

import AudioFile.Frames
import java.io.IOException
import scala.{ Byte => SByte, Double => SDouble, Float => SFloat, Int => SInt, Short => SShort }
import java.nio.channels.{ ReadableByteChannel, WritableByteChannel }
import math._
import java.nio._

trait BufferHandler {
   protected def byteBuf : ByteBuffer
   def numChannels : SInt
   protected def bitsPerSample : SInt
   val frameSize : SInt = (bitsPerSample >> 3) * numChannels  // bytes per frame
   protected val bufFrames = byteBuf.capacity / frameSize

   protected def checkCapacity = require( bufFrames > 0, "Buffer too small" )
}

trait BufferReader extends BufferHandler {
   @throws( classOf[ IOException ])
   def readFrames( frames: Frames, off: SInt, len: SInt ) : Unit
   protected val read : ReadableByteChannel
}

trait BufferWriter extends BufferHandler {
   @throws( classOf[ IOException ])
   def writeFrames( frames: Frames, off: SInt, len: SInt ) : Unit
   protected val write : WritableByteChannel
}

trait BufferBidi extends BufferReader with BufferWriter

// -------------- Basic --------------

object BufferHandler {
   abstract class Byte extends BufferHandler {
      protected val bitsPerSample   = 8
      protected val arrayBuf        = new Array[ SByte ]( byteBuf.capacity() )
   }

   abstract class UByte extends BufferHandler {
      protected val bitsPerSample   = 8
      protected val arrayBuf        = new Array[ SByte ]( byteBuf.capacity() )
   }

   abstract class Short extends BufferHandler {
      protected val bitsPerSample   = 16
      byteBuf.clear
      protected val viewBuf	      = byteBuf.asShortBuffer()
      protected val arrayBuf	      = new Array[ SShort ]( viewBuf.capacity() )
   }

   abstract class ThreeBytes extends BufferHandler {
      protected val bitsPerSample   = 24
      // note : it's *not* faster to use ByteBuffer.allocate()
      // and ByteBuffer.array() than this implementation
      // (using ByteBuffer.allocateDirect() and bulk get into a separate arrayBuf)
      protected val arrayBuf        = new Array[ SByte ]( byteBuf.capacity() )
      protected val chStep          = numChannels * 3
   }

   abstract class Int extends BufferHandler {
      protected val bitsPerSample   = 32
      byteBuf.clear()
      protected val viewBuf         = byteBuf.asIntBuffer()
      protected val arrayBuf	      = new Array[ SInt ]( viewBuf.capacity() )
   }

   abstract class Float extends BufferHandler {
      protected val bitsPerSample   = 32
      byteBuf.clear()
      protected val viewBuf	      = byteBuf.asFloatBuffer()
      protected val arrayBuf	      = new Array[ SFloat ]( viewBuf.capacity() )
   }

   abstract class Double extends BufferHandler {
      protected val bitsPerSample   = 64
      byteBuf.clear()
      protected val viewBuf	      = byteBuf.asDoubleBuffer()
      protected val arrayBuf	      = new Array[ SDouble ]( viewBuf.capacity() )
   }
}

// -------------- Reader --------------

trait BufferReaderFactory {
   def apply( read: ReadableByteChannel, byteBuf: ByteBuffer, numChannels: SInt ) : BufferReader
}
object BufferReader {
   object Byte extends BufferReaderFactory
   case class Byte( read: ReadableByteChannel, byteBuf: ByteBuffer, numChannels: SInt )
   extends BufferHandler.Byte with ByteLike {
      checkCapacity
   }

   // float to byte = f*0x7F+0x80 (-1 ... +1becomes 0x01 to 0xFF)
   // which is how libsndfile behaves
   object UByte extends BufferReaderFactory
   case class UByte( read: ReadableByteChannel, byteBuf: ByteBuffer, numChannels: SInt )
   extends BufferHandler.UByte with UByteLike {
      checkCapacity
   }

   object Short extends BufferReaderFactory
   case class Short( read: ReadableByteChannel, byteBuf: ByteBuffer, numChannels: SInt )
   extends BufferHandler.Short with ShortLike {
      checkCapacity
   }

   object ThreeBytes extends BufferReaderFactory {
      def apply( read: ReadableByteChannel, byteBuf: ByteBuffer,
                 numChannels: SInt ) : BufferReader = {
         if( byteBuf.order() == ByteOrder.LITTLE_ENDIAN ) {
            new ThreeBytesLE( read, byteBuf, numChannels )
         } else {
            new ThreeBytesBE( read, byteBuf, numChannels )
         }
      }
   }

   /*
    *  24bit big endian
    */
   case class ThreeBytesBE( read: ReadableByteChannel, byteBuf: ByteBuffer, numChannels: SInt )
   extends BufferHandler.ThreeBytes with ThreeBytesBELike {
      checkCapacity
   }

   /*
    *  24bit little endian
    */
   case class ThreeBytesLE( read: ReadableByteChannel, byteBuf: ByteBuffer, numChannels: SInt )
   extends BufferHandler.ThreeBytes with ThreeBytesLELike {
      checkCapacity
   }

   object Int extends BufferReaderFactory
   case class Int( read: ReadableByteChannel, byteBuf: ByteBuffer, numChannels: SInt )
   extends BufferHandler.Int with IntLike {
      checkCapacity
   }

   object Float extends BufferReaderFactory
   case class Float( read: ReadableByteChannel, byteBuf: ByteBuffer, numChannels: SInt )
   extends BufferHandler.Float with FloatLike {
      checkCapacity
   }

   object Double extends BufferReaderFactory
   case class Double( read: ReadableByteChannel, byteBuf: ByteBuffer, numChannels: SInt )
   extends BufferHandler.Double with DoubleLike {
      checkCapacity
   }

   trait ByteLike extends BufferReader {
      me: BufferHandler.Byte =>

      def readFrames( frames: Frames, off: SInt, len: SInt ) {
         var remaining  = len
         var position   = off
         while( remaining > 0 ) {
            val chunkLen   = math.min( bufFrames, remaining )
            val m			   = chunkLen * frameSize
            byteBuf.rewind().limit( m )
            read.read( byteBuf )
            byteBuf.flip()
            byteBuf.get( arrayBuf, 0, m )
            var ch = 0; while( ch < numChannels ) {
               val b = frames( ch )
               if( b != null ) {
                  var i = ch; var j = position; while( i < m ) {
                     b( j ) = arrayBuf( i ).toFloat / 0x7F
                     i += numChannels; j += 1
                  }
               }
               ch += 1
            }
            remaining -= chunkLen
            position  += chunkLen
         }
      }
   }

   trait UByteLike extends BufferReader {
      me: BufferHandler.UByte =>
      
      def readFrames( frames: Frames, off: SInt, len: SInt ) {
         var remaining  = len
         var position   = off
         while( remaining > 0 ) {
            val chunkLen   = min( bufFrames, remaining )
            val m          = chunkLen * frameSize
            byteBuf.rewind().limit( m )
            read.read( byteBuf )
            byteBuf.flip()
            byteBuf.get( arrayBuf, 0, m )
            var ch = 0; while( ch < numChannels ) {
               val b = frames( ch )
               if( b != null ) {
                  var i = ch; var j = position; while( i < m ) {
                     val arr1 = arrayBuf( i )
                     // hmmm, java can't handle unsigned bytes
                     b( j ) = (if( arr1 < 0 ) (0x80 + arr1) else (arr1 - 0x80)).toFloat / 0x7F
                  }
                  i += numChannels; j += 1
               }
               ch += 1
            }
            remaining -= chunkLen
            position  += chunkLen
         }
      }
   }

   trait ShortLike extends BufferReader {
      me: BufferHandler.Short =>

      def readFrames( frames: Frames, off: SInt, len: SInt ) {
         var remaining  = len
         var position   = off
         while( remaining > 0 ) {
            val chunkLen   = min( bufFrames, remaining )
            val m			   = chunkLen * numChannels
            byteBuf.rewind().limit( chunkLen * frameSize )
            read.read( byteBuf )
            viewBuf.clear()
            viewBuf.get( arrayBuf, 0, m )
            var ch = 0; while( ch < numChannels ) {
               val b = frames( ch )
               if( b != null ) {
                  var i = ch; var j = position; while( i < m ) {
                     b( j ) = arrayBuf( i ).toFloat / 0x7FFF
                     i += numChannels; j += 1
                  }
               }
               ch += 1
            }
            remaining -= chunkLen
            position  += chunkLen
         }
      }
   }

   trait ThreeBytesBELike extends BufferReader {
      me: BufferHandler.ThreeBytes =>

      def readFrames( frames: Frames, off: SInt, len: SInt ) {
         var remaining  = len
         var position   = off
         while( remaining > 0 ) {
            val chunkLen   = min( bufFrames, remaining )
            val m			   = chunkLen * frameSize
            byteBuf.rewind().limit( m )
            read.read( byteBuf )
            byteBuf.flip()
            byteBuf.get( arrayBuf, 0, m )
            var ch = 0; var p = 0; while( ch < numChannels ) {
               val b = frames( ch )
               if( b != null ) {
                  var i = p; var j = position; while( i < m ) {
                     b( j ) = ((arrayBuf( i ) << 16 ) |
                              ((arrayBuf( i + 1 ) & 0xFF) << 8) |
                               (arrayBuf( i + 2 ) & 0xFF)).toFloat / 0x7FFFFF
                     i += chStep; j += 1
                  }
               }
               ch += 1; p += 3
            }
            remaining -= chunkLen
            position  += chunkLen
         }
      }
   }

   trait ThreeBytesLELike extends BufferReader {
      me: BufferHandler.ThreeBytes =>

      def readFrames( frames: Frames, off: SInt, len: SInt ) {
         var remaining  = len
         var position   = off
         while( remaining > 0 ) {
            val chunkLen   = min( bufFrames, remaining )
            val m			   = chunkLen * frameSize
            byteBuf.rewind().limit( m )
            read.read( byteBuf )
            byteBuf.flip()
            byteBuf.get( arrayBuf, 0, m )
            var ch = 0; var p = 0; while( ch < numChannels ) {
               val b = frames( ch )
               if( b != null ) {
                  var i = p; var j = position; while( i < m ) {
                     b( j ) = ((arrayBuf( i ) & 0xFF)|
                              ((arrayBuf( i + 1 ) & 0xFF) << 8) |
                               (arrayBuf( i + 2 ) << 16 )).toFloat / 0x7FFFFF
                     i += chStep; j += 1
                  }
               }
               ch += 1; p += 3
            }
            remaining -= chunkLen
            position  += chunkLen
         }
      }
   }

   trait IntLike extends BufferReader {
      me: BufferHandler.Int =>

      def readFrames( frames: Frames, off: SInt, len: SInt ) {
         var remaining  = len
         var position   = off
         while( remaining > 0 ) {
            val chunkLen   = min( bufFrames, remaining )
            val m			   = chunkLen * numChannels
            byteBuf.rewind().limit( chunkLen * frameSize )
            read.read( byteBuf )
            viewBuf.clear()
            viewBuf.get( arrayBuf, 0, m )
            var ch = 0; while( ch < numChannels ) {
               val b = frames( ch )
               if( b != null ) {
                  var i = ch; var j = position; while( i < m ) {
                     b( j ) = arrayBuf( i ).toFloat / 0x7FFFFFFF
                     i += numChannels; j += 1
                  }
               }
               ch += 1
            }
            remaining -= chunkLen
            position  += chunkLen
         }
      }
   }

   trait FloatLike extends BufferReader {
      me: BufferHandler.Float =>

      def readFrames( frames: Frames, off: SInt, len: SInt ) {
         var remaining  = len
         var position   = off
         while( remaining > 0 ) {
            val chunkLen   = min( bufFrames, remaining )
            val m          = chunkLen * numChannels
            byteBuf.rewind().limit( chunkLen * frameSize )
            read.read( byteBuf )
            viewBuf.clear()
            viewBuf.get( arrayBuf, 0, m )
            var ch = 0; while( ch < numChannels ) {
               val b = frames( ch )
               if( b != null ) {
                  var i = ch; var j = position; while( i < m ) {
                     b( j ) = arrayBuf( i )
                     i += numChannels; j += 1
                  }
               }
               ch += 1
            }
            remaining -= chunkLen
            position  += chunkLen
         }
      }
   }

   trait DoubleLike extends BufferReader {
      me: BufferHandler.Double =>

      def readFrames( frames: Frames, off: SInt, len: SInt ) {
         var remaining  = len
         var position   = off
         while( remaining > 0 ) {
            val chunkLen   = min( bufFrames, remaining )
            val m			   = chunkLen * numChannels
            byteBuf.rewind().limit( chunkLen * frameSize )
            read.read( byteBuf )
            viewBuf.clear()
            viewBuf.get( arrayBuf, 0, m )
            var ch = 0; while( ch < numChannels ) {
               val b = frames( ch )
               if( b != null ) {
                  var i = ch; var j = position; while( i < m ) {
                     b( j ) = arrayBuf( i ).toFloat
                     i += numChannels; j += 1
                  }
               }
               ch += 1
            }
            remaining -= chunkLen
            position  += chunkLen
         }
      }
   }
}

// -------------- Write --------------

trait BufferWriterFactory {
   def apply( write: WritableByteChannel, byteBuf: ByteBuffer, numChannels: SInt ) : BufferWriter
}
object BufferWriter {
   object Byte extends BufferWriterFactory
   case class Byte( write: WritableByteChannel, byteBuf: ByteBuffer,numChannels: SInt )
   extends BufferHandler.Byte with ByteLike {
      checkCapacity
   }

   // float to byte = f*0x7F+0x80 (-1 ... +1becomes 0x01 to 0xFF)
   // which is how libsndfile behaves
   object UByte extends BufferWriterFactory
   case class UByte( write: WritableByteChannel, byteBuf: ByteBuffer, numChannels: SInt )
   extends BufferHandler.UByte with UByteLike {
      checkCapacity
   }

   object Short extends BufferWriterFactory
   case class Short( write: WritableByteChannel, byteBuf: ByteBuffer, numChannels: SInt )
   extends BufferHandler.Short with ShortLike {
      checkCapacity
   }

   object ThreeBytes extends BufferWriterFactory {
      def apply( write: WritableByteChannel, byteBuf: ByteBuffer, numChannels: SInt ) : BufferWriter = {
         if( byteBuf.order() == ByteOrder.LITTLE_ENDIAN ) {
            new ThreeBytesLE( write, byteBuf, numChannels )
         } else {
            new ThreeBytesBE( write, byteBuf, numChannels )
         }
      }
   }

   case class ThreeBytesBE( write: WritableByteChannel, byteBuf: ByteBuffer, numChannels: SInt )
   extends BufferHandler.ThreeBytes with ThreeBytesBELike {
      checkCapacity
   }

   case class ThreeBytesLE( write: WritableByteChannel, byteBuf: ByteBuffer, numChannels: SInt )
   extends BufferHandler.ThreeBytes with ThreeBytesLELike {
      checkCapacity
   }

   object Int extends BufferWriterFactory
   case class Int( write: WritableByteChannel, byteBuf: ByteBuffer, numChannels: SInt )
   extends BufferHandler.Int with IntLike {
      checkCapacity
   }

   object Float extends BufferWriterFactory
   case class Float( write: WritableByteChannel, byteBuf: ByteBuffer, numChannels: SInt )
   extends BufferHandler.Float with FloatLike {
      checkCapacity
   }

   object Double extends BufferWriterFactory
   case class Double( write: WritableByteChannel, byteBuf: ByteBuffer, numChannels: SInt )
   extends BufferHandler.Double with DoubleLike {
      checkCapacity
   }

   trait ByteLike extends BufferWriter {
      me: BufferHandler.Byte =>
      
      def writeFrames( frames: Frames, off: SInt, len: SInt ) {
         var remaining  = len
         var position   = off
         while( remaining > 0 ) {
            val chunkLen   = math.min( bufFrames, remaining )
            val m			   = chunkLen * frameSize
            var ch = 0; while( ch < numChannels ) {
               val b = frames( ch )
               var i = ch; var j = position; while( i < m ) {
                  arrayBuf( i ) = (b( j ) * 0x7F).toByte
                  i += numChannels; j += 1
               }
               ch += 1
            }
            byteBuf.clear()
            byteBuf.put( arrayBuf, 0, m )
            byteBuf.flip()
            write.write( byteBuf )
            remaining -= chunkLen
            position  += chunkLen
         }
      }
   }

   trait UByteLike extends BufferWriter {
      me: BufferHandler.UByte =>
      
      def writeFrames( frames: Frames, off: SInt, len: SInt ) {
         var remaining  = len
         var position   = off
         while( remaining > 0 ) {
            val chunkLen   = min( bufFrames, remaining )
            val m			   = chunkLen * frameSize
            var ch = 0; while( ch < numChannels ) {
               val b = frames( ch )
               var i = ch; var j = position; while( i < m ) {
                  arrayBuf( i ) = (b( j ) * 0x7F + 0x80).toByte
                  i += numChannels; j += 1
               }
               ch += 1
            }
            byteBuf.clear()
            byteBuf.put( arrayBuf, 0, m )
            byteBuf.flip()
            write.write( byteBuf )
            remaining -= chunkLen
            position  += chunkLen
         }
      }
   }

   trait ShortLike extends BufferWriter {
      me: BufferHandler.Short =>

      def writeFrames( frames: Frames, off: SInt, len: SInt ) {
         var remaining  = len
         var position   = off
         while( remaining > 0 ) {
            val chunkLen   = min( bufFrames, remaining )
            val m			   = chunkLen * numChannels
            var ch = 0; while( ch < numChannels ) {
               val b = frames( ch )
               if( b != null ) {
                  var i = ch; var j = position; while( i < m ) {
                     arrayBuf( i ) = (b( j ) * 0x7FFF).toShort
                     i += numChannels; j += 1
                  }
               }
               ch += 1
            }
            viewBuf.clear()
            viewBuf.put( arrayBuf, 0, m )
            byteBuf.rewind().limit( chunkLen * frameSize )
            write.write( byteBuf )
            remaining -= chunkLen
            position  += chunkLen
         }
      }
   }

   trait ThreeBytesBELike extends BufferWriter {
      me: BufferHandler.ThreeBytes =>

      def writeFrames( frames: Frames, off: SInt, len: SInt ) {
         var remaining  = len
         var position   = off
         while( remaining > 0 ) {
            val chunkLen   = min( bufFrames, remaining )
            val m			   = chunkLen * frameSize
            var ch = 0; var p = 0; while( ch < numChannels ) {
               val b = frames( ch )
               var i = p; var j = position; while( i < m ) {
                  val k = (b( j ) * 0x7FFFFF).toInt
                  arrayBuf( i )     = (k >> 16).toByte
                  arrayBuf( i + 1 ) = (k >> 8).toByte
                  arrayBuf( i + 2 ) = k.toByte
                  i += chStep; j += 1
               }
               ch += 1; p += 3
            }
            byteBuf.clear()
            byteBuf.put( arrayBuf, 0, m )
            byteBuf.flip()
            write.write( byteBuf )
            remaining -= chunkLen
            position  += chunkLen
         }
      }
   }

   trait ThreeBytesLELike extends BufferWriter {
      me: BufferHandler.ThreeBytes =>

      def writeFrames( frames: Frames, off: SInt, len: SInt ) {
         var remaining  = len
         var position   = off
         while( remaining > 0 ) {
            val chunkLen   = min( bufFrames, remaining )
            val m			   = chunkLen * frameSize
            var ch = 0; var p = 0; while( ch < numChannels ) {
               val b = frames( ch )
               if( b != null ) {
                  var i = p; var j = position; while( i < m ) {
                     val k = (b( j ) * 0x7FFFFF).toInt
                     arrayBuf( i )     = k.toByte
                     arrayBuf( i + 1 ) = (k >> 8).toByte
                     arrayBuf( i + 2 ) = (k >> 16).toByte
                     i += chStep; j += 1
                  }
               }
               ch += 1; p += 3
            }
            byteBuf.clear()
            byteBuf.put( arrayBuf, 0, m )
            byteBuf.flip()
            write.write( byteBuf )
            remaining -= chunkLen
            position  += chunkLen
         }
      }
   }

   trait IntLike extends BufferWriter {
      me: BufferHandler.Int =>

      def writeFrames( frames: Frames, off: SInt, len: SInt ) {
         var remaining  = len
         var position   = off
         while( remaining > 0 ) {
            val chunkLen   = min( bufFrames, remaining )
            val m	         = chunkLen * numChannels
            var ch = 0; while( ch < numChannels ) {
               val b = frames( ch )
               if( b != null ) {
                  var i = ch; var j = position; while( i < m ) {
                     arrayBuf( i ) = (b( j ) * 0x7FFFFFFF).toInt
                     i += numChannels; j += 1
                  }
               }
               ch += 1
            }
            viewBuf.clear()
            viewBuf.put( arrayBuf, 0, m )
            byteBuf.rewind().limit( chunkLen * frameSize )
            write.write( byteBuf )
            remaining -= chunkLen
            position  += chunkLen
         }
      }
   }

   trait FloatLike extends BufferWriter {
      me: BufferHandler.Float =>

      def writeFrames( frames: Frames, off: SInt, len: SInt ) {
         var remaining  = len
         var position   = off
         while( remaining > 0 ) {
            val chunkLen   = min( bufFrames, remaining )
            val m			   = chunkLen * numChannels
            var ch = 0; while( ch < numChannels ) {
               val b = frames( ch )
               var i = ch; var j = position; while( i < m ) {
                  arrayBuf( i ) = b( j )
                  i += numChannels; j += 1
               }
               ch += 1
            }
            viewBuf.clear()
            viewBuf.put( arrayBuf, 0, m )
            byteBuf.rewind().limit( chunkLen * frameSize )
            write.write( byteBuf )
            remaining -= chunkLen
            position  += chunkLen
         }
      }
   }

   trait DoubleLike extends BufferWriter {
      me: BufferHandler.Double =>

      def writeFrames( frames: Frames, off: SInt, len: SInt ) {
         var remaining  = len
         var position   = off
         while( remaining > 0 ) {
            val chunkLen   = min( bufFrames, remaining )
            val m			   = chunkLen * numChannels
            var ch = 0; while( ch < numChannels ) {
               val b = frames( ch )
               if( b != null ) {
                  var i = ch; var j = position; while( i < m ) {
                     arrayBuf( i ) = b( j )
                     i += numChannels; j += 1
                  }
               }
               ch += 1
            }
            viewBuf.clear()
            viewBuf.put( arrayBuf, 0, m )
            byteBuf.rewind().limit( chunkLen * frameSize )
            write.write( byteBuf )
            remaining -= chunkLen
            position  += chunkLen
         }
      }
   }
}

// -------------- Handler -------------- 

trait BufferBidiFactory {
   def apply( read: ReadableByteChannel, write: WritableByteChannel, byteBuf: ByteBuffer,
              numChannels: SInt ) : BufferBidi
}
object BufferBidi {
   object Byte extends BufferBidiFactory
   case class Byte( read: ReadableByteChannel, write: WritableByteChannel, byteBuf: ByteBuffer, numChannels: SInt )
   extends BufferHandler.Byte with BufferReader.ByteLike with BufferWriter.ByteLike with BufferBidi {
      checkCapacity
   }

   object UByte extends BufferBidiFactory
   case class UByte( read: ReadableByteChannel, write: WritableByteChannel, byteBuf: ByteBuffer, numChannels: SInt )
   extends BufferHandler.UByte with BufferReader.UByteLike with BufferWriter.UByteLike with BufferBidi {
      checkCapacity
   }

   object Short extends BufferBidiFactory
   case class Short( read: ReadableByteChannel, write: WritableByteChannel, byteBuf: ByteBuffer, numChannels: SInt )
   extends BufferHandler.Short with BufferReader.ShortLike with BufferWriter.ShortLike with BufferBidi {
      checkCapacity
   }

   object ThreeBytes extends BufferBidiFactory {
      def apply( read: ReadableByteChannel, write: WritableByteChannel, byteBuf: ByteBuffer,
                 numChannels: SInt ) : BufferBidi = {
         if( byteBuf.order() == ByteOrder.LITTLE_ENDIAN ) {
            new ThreeBytesLE( read, write, byteBuf, numChannels )
         } else {
            new ThreeBytesBE( read, write, byteBuf, numChannels )
         }
      }
   }

   case class ThreeBytesBE( read: ReadableByteChannel, write: WritableByteChannel,
                            byteBuf: ByteBuffer, numChannels: SInt )
   extends BufferHandler.ThreeBytes with BufferReader.ThreeBytesBELike with BufferWriter.ThreeBytesBELike
   with BufferBidi {
      checkCapacity
   }

   case class ThreeBytesLE( read: ReadableByteChannel, write: WritableByteChannel,
                            byteBuf: ByteBuffer, numChannels: SInt )
   extends BufferHandler.ThreeBytes with BufferReader.ThreeBytesLELike with BufferWriter.ThreeBytesLELike
   with BufferBidi {
      checkCapacity
   }

   object Int extends BufferBidiFactory
   case class Int( read: ReadableByteChannel, write: WritableByteChannel, byteBuf: ByteBuffer, numChannels: SInt )
   extends BufferHandler.Int with BufferReader.IntLike with BufferWriter.IntLike with BufferBidi {
      checkCapacity
   }

   object Float extends BufferBidiFactory
   case class Float( read: ReadableByteChannel, write: WritableByteChannel, byteBuf: ByteBuffer, numChannels: SInt )
   extends BufferHandler.Float with BufferReader.FloatLike with BufferWriter.FloatLike with BufferBidi {
      checkCapacity
   }

   object Double extends BufferBidiFactory
   case class Double( read: ReadableByteChannel, write: WritableByteChannel, byteBuf: ByteBuffer, numChannels: SInt )
   extends BufferHandler.Double with BufferReader.DoubleLike with BufferWriter.DoubleLike with BufferBidi {
      checkCapacity
   }
}