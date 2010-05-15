package de.sciss.synth.io

import AudioFile.Frames
import java.io.IOException
import scala.{ Byte => SByte, Double => SDouble, Float => SFloat, Int => SInt, Short => SShort }
import java.nio.channels.{WritableByteChannel, ReadableByteChannel}
import java.nio.{IntBuffer, ShortBuffer, ByteBuffer}

trait BufferReader {
   @throws( classOf[ IOException ])
   def readFrames( frames: Frames, off: SInt, len: SInt ) : Unit

   protected val read : ReadableByteChannel
   protected val byteBuf : ByteBuffer
}

trait BufferWriter {
   @throws( classOf[ IOException ])
   def writeFrames( frames: Frames, off: SInt, len: SInt ) : Unit

   protected val write : WritableByteChannel
   protected val byteBuf : ByteBuffer
}

trait BufferHandler extends BufferReader with BufferWriter

// -------------- Reader --------------

object BufferReader {
   class Byte( protected val read: ReadableByteChannel, protected val byteBuf: ByteBuffer )
   extends ByteLike {
      protected val arrayBuf = new Array[ SByte ]( byteBuf.capacity() ))
   }

   // float to byte = f*0x7F+0x80 (-1 ... +1becomes 0x01 to 0xFF)
   // which is how libsndfile behaves
   class UByte( protected val read: ReadableByteChannel, protected val byteBuf: ByteBuffer )
   extends UByteLike {
      protected val arrayBuf = new Array[ SByte ]( byteBuf.capacity() )
   }

   class Short( protected val read: ReadableByteChannel, protected val byteBuf: ByteBuffer )
   extends ShortLike {
      byteBuf.clear
      protected val viewBuf	= byteBuf.asShortBuffer()
      protected val arrayBuf	= new Array[ SShort ]( viewBuf.capacity() )
   }

   /*
    *  24bit big endian
    */
   class ThreeByte( protected val read: ReadableByteChannel, protected val byteBuf: ByteBuffer )
   extends ThreeByteLike {
      // note : it's *not* faster to use ByteBuffer.allocate()
      // and ByteBuffer.array() than this implementation
      // (using ByteBuffer.allocateDirect() and bulk get into a separate arrayBuf)
      protected val arrayBuf = new Array[ Byte ]( byteBuf.capacity() )
      protected val chStep   = numChannels * 3
   }

   /*
    *  24bit little endian
    */
   class ThreeLittleByte( protected val read: ReadableByteChannel, protected val byteBuf: ByteBuffer )
   extends ThreeLittleByteLike {
      // note : it's *not* faster touse ByteBuffer.allocate()
      // and ByteBuffer.array() than this implementation
      // (using ByteBuffer.allocateDirect() and bulk get into a separate arrayBuf)
      protected val arrayBuf = new Array[ Byte ]( byteBuf.capacity() )
      protected val chStep   = numChannels * 3
   }

   class Int( protected val read: ReadableByteChannel, protected val byteBuf: ByteBuffer )
   extends IntLike {
      byteBuf.clear();
      protected val viewBuf   = byteBuf.asIntBuffer()
      protected val arrayBuf	= new Array[ SInt ]( viewBuf.capacity() )
   }

   class Float( protected val read: ReadableByteChannel, protected val byteBuf: ByteBuffer )
   extends FloatLike {
      byteBuf.clear()
      protected val viewBuf	= byteBuf.asFloatBuffer()
      protected val arrayBuf	= new Array[ SFloat ]( viewBuf.capacity() )
   }

   class Double( protected val read: ReadableByteChannel, protected val byteBuf: ByteBuffer )
   extends DoubleLike {
      byteBuf.clear()
      protected val viewBuf	= byteBuf.asDoubleBuffer()
      protected val arrayBuf	= new Array[ SDouble ]( viewBuf.capacity() )
   }

   trait ByteLike extends BufferReader {
      protected val arrayBuf : Array[ SByte ]

      def readFrames( frames: Frames, off: SInt, len: SInt ) {
         while( len > 0 ) {
            val chunkLen   = math.min( frameBufCapacity, len );
            val m			   = chunkLen * bytesPerFrame;
            byteBuf.rewind().limit( m )
            read.read( byteBuf )
            byteBuf.flip()
            byteBuf.get( arrayBuf, 0, m )
            var ch = 0; while( ch < numChannels ) {
               val b = frames( ch )
               if( b != null ) {
                  var i = ch; var j = off; while( i < m ) {
                     b( j ) = arrayBuf( i ).toFloat / 0x7F
                     i += numChannels; j += 1
                  }
               }
               ch += 1
            }
            len -= chunkLen
            off += chunkLen
         }
      }
   }

   trait UByteLike extends BufferReader {
      protected val arrayBuf : Array[ SByte ]

      protected def readFrames( frames: Frames, off: SInt, len: SInt ) {
         while( len > 0 ){
            val chunkLen   = min( frameBufCapacity, len )
            val m          = chunkLen * bytesPerFrame;
            byteBuf.rewind().limit( m )
            read.read( byteBuf )
            byteBuf.flip()
            byteBuf.get( arrayBuf, 0, m )
            var ch = 0; while( ch < numChannels ) {
               val b = frames( ch )
               if( b != null ) {
                  var i = ch; var j = off; while( i < m ) {
                     val arr1 = arrayBuf( i )
                     // hmmm, java can't handle unsigned bytes
                     b( j ) = (if( arr1 < 0 ) (0x80 + arr1) else (arr1 - 0x80)).toFloat / 0x7F
                  }
                  i += numChannels; j += 1
               }
               ch += 1
            }
            len -= chunkLen
            off += chunkLen
         }
      }
   }

   trait ShortLike extends BufferReader {
      protected val viewBuf : ShortBuffer
      protected val arrayBuf : Array[ SShort ]

      protected def readFrames( frames: Frames, off: SInt, len: SInt ) {
         while( len > 0 ) {
            val chunkLen   = min( frameBufCapacity, len )
            val m			   = chunkLen * numChannels
            byteBuf.rewind().limit( chunkLen * bytesPerFrame )
            read.read( byteBuf )
            viewBuf.clear()
            viewBuf.get( arrayBuf, 0, m )
            var ch = 0; while( ch < numChannels ) {
               val b = frames( ch )
               if( b != null ) {
                  var i = ch; var j = off; while( i < m ) {
                     b( j ) = arrayBuf( i ).toFloat / 0x7FFF
                     i += numChannels; j += 1
                  }
               }
               ch += 1
            }
            len -= chunkLen
            off += chunkLen
         }
      }
   }

   trait ThreeByteLike extends BufferReader {
      protected val arrayBuf : Array[ SByte ]
      protected val chStep : SInt

      protected def readFrames( frames: Frames, off: SInt, len: SInt ) {
         while( len > 0 ) {
            val chunkLen   = min( frameBufCapacity, len )
            val m			      = chunkLen * bytesPerFrame
            byteBuf.rewind().limit( m )
            read.read( byteBuf )
            byteBuf.flip()
            byteBuf.get( arrayBuf,0, m )
            var ch = 0; while( ch < numChannels ) {
               val b = frames( ch )
               if( b != null ) {
                  var i = ch * 3; var j = off; while( i < m ) {
                     b( j ) = ((arrayBuf( i ) << 16 ) |
                              ((arrayBuf( i + 1 ) & 0xFF) << 8) |
                               (arrayBuf( i + 2 ) & 0xFF)).toFloat / 0x7FFFFF
                     i += chStep; j += 1
                  }
               }
               ch += 1
            }
            len -= chunkLen
            off += chunkLen
         }
      }
   }

   trait ThreeLittleByteLike extends BufferReader {
      protected val arrayBuf : Array[ SByte ]
      protected val chStep : SInt

      protected def readFrames( frames: Frames, off: SInt, len: SInt ) {
         while( len > 0) {
            val chunkLen   = min( frameBufCapacity, len )
            val m			      = chunkLen * bytesPerFrame
            byteBuf.rewind().limit( m )
            fch.read( byteBuf )
            byteBuf.flip()
            byteBuf.get( arrayBuf, 0, m )
            var ch = 0; var p = 0; while( ch < numChannels ) {
               val b = frames( ch )
               if( b != null ) {
                  var i = p; j = off; while( i < m ) {
                     b( j ) = ((arrayBuf( i ) & 0xFF)|
                              ((arrayBuf( i + 1 ) & 0xFF) << 8) |
                               (arrayBuf( i + 2 ) << 16 )).toFloat / 0x7FFFFF
                     i += chStep; j += 1
                  }
               }
               ch += 1; p += 3
            }
            len -= chunkLen
            off += chunkLen
         }
      }
   }

   trait IntLike extends BufferReader {
      protected val viewBuf : IntBuffer
      protected val arrayBuf : Array[ SInt ]

      protected def readFrames( frames: Frames, off: SInt, len: SInt ) {
         while( len > 0 ) {
            val chunkLen   = min( frameBufCapacity, len )
            val m			   = chunkLen * numChannels
            byteBuf.rewind().limit( chunkLen * bytesPerFrame )
            read.read( byteBuf )
            viewBuf.clear()
            viewBuf.get( arrayBuf, 0, m )
            var ch = 0; while( ch < numChannels ) {
               val b = frames( ch )
               if( b != null ) {
                  var i = ch; var j = off; while( i < m ) {
                     b( j ) = arrayBuf( i ).toFloat / 0x7FFFFFFF
                     i += numChannels; j += 1
                  }
               }
               ch += 1
            }
            len -= chunkLen
            off += chunkLen
         }
      }
   }

   trait FloatLike extends BufferReader {
      protected val viewBuf : IntBuffer
      protected val arrayBuf : Array[ SFloat ]

      protected def readFrames( frames: Frames, off: SInt, len: SInt ) {
         while( len > 0 ) {
            val chunkLen   = min( frameBufCapacity, len )
            val m          = chunkLen * numChannels
            byteBuf.rewind().limit( chunkLen * bytesPerFrame )
            read.read( byteBuf )
            viewBuf.clear()
            viewBuf.get( arrayBuf, 0, m )
            var ch = 0; while( ch < numChannels ) {
               val b = frames( ch )
               if( b != null ) {
                  var i = ch; var j = off; while( i < m ) {
                     b( j ) = arrayBuf( i )
                     i += numChannels; j += 1
                  }
               }
               ch += 1
            }
            len -= chunkLen
            off += chunkLen
         }
      }
   }

   trait DoubleLike extends BufferReader {
      protected val viewBuf : IntBuffer
      protected val arrayBuf : Array[ SDouble ]

      protected def readFrames( frames: Frames, off: SInt, len: SInt ) {
         while( len > 0 ) {
            val chunkLen   = min( frameBufCapacity, len )
            val m			   = chunkLen * numChannels
            byteBuf.rewind().limit( chunkLen * bytesPerFrame )
            read.read( byteBuf )
            viewBuf.clear()
            viewBuf.get( arrayBuf, 0, m )
            var ch = 0; while( ch < numChannels ) {
               val b = frames( ch )
               if( b != null ) {
                  var i = ch; var j = off; while( i < m ) {
                     b( j ) = arrayBuf( i ).toFloat
                     i += numChannels; j += 1
                  }
               }
               ch += 1
            }
            len -= chunkLen
            off += chunkLen
         }
      }
   }
}

// -------------- Write --------------

object BufferWriter {
   class Byte( protected val write: WritableByteChannel, protected val byteBuf: ByteBuffer )
   extends ByteLike {
      protected val arrayBuf = new Array[ SByte ]( byteBuf.capacity() )
   }

   // float to byte = f*0x7F+0x80 (-1 ... +1becomes 0x01 to 0xFF)
   // which is how libsndfile behaves
   class UByte( protected val write: WritableByteChannel, protected val byteBuf: ByteBuffer )
   extends UByteLike {
      protected val arrayBuf = new Array[ SByte ]( byteBuf.capacity() )
   }

   class Short( protected val write: WritableByteChannel, protected val byteBuf: ByteBuffer )
   extends ShortLike {
      byteBuf.clear
      protected val viewBuf	= byteBuf.asShortBuffer()
      protected val arrayBuf	= new Array[ SShort ]( viewBuf.capacity() )
   }

   class ThreeByte( protected val write: WritableByteChannel, protected val byteBuf: ByteBuffer )
   extends ThreeByteLike {
      protected val arrayBuf = new Array[ Byte ]( byteBuf.capacity() )
      protected val chStep   = numChannels * 3
   }

   class ThreeLittleByte( protected val write: WritableByteChannel, protected val byteBuf: ByteBuffer )
   extends ThreeLittleByteLike {
      protected val arrayBuf = new Array[ Byte ]( byteBuf.capacity() )
      protected val chStep   = numChannels * 3
   }

   class Int( protected val write: WritableByteChannel, protected val byteBuf: ByteBuffer )
   extends IntLike {
      byteBuf.clear
      protected val viewBuf	= byteBuf.asIntBuffer()
      protected val arrayBuf	= new Array[ SInt ]( viewBuf.capacity() )
   }

   class Float( protected val write: WritableByteChannel, protected val byteBuf: ByteBuffer )
   extends FloatLike {
      byteBuf.clear()
      protected val viewBuf	= byteBuf.asFloatBuffer()
      protected val arrayBuf	= new Array[ SFloat ]( viewBuf.capacity() )
   }

   class Double( protected val write: WritableByteChannel, protected val byteBuf: ByteBuffer )
   extends DoubleLike {
      byteBuf.clear()
      protected val viewBuf	= byteBuf.asDoubleBuffer()
      protected val arrayBuf	= new Array[ SDouble ]( viewBuf.capacity() )
   }

   trait ByteLike extends BufferWriter {
      protected val arrayBuf : Array[ SByte ]

      protected def writeFrames( frames: Frames, off: Int, len: Int ) {
         while( len > 0 ) {
            val chunkLen   = math.min( frameBufCapacity, len );
            val m			   = chunkLen * bytesPerFrame;
            var ch = 0; while( ch < numChannels ) {
               val b = frames( ch )
               var i = ch; var j = off; while( i < m ) {
                  arrayBuf( i ) = (b( j ) * 0x7F).toByte
                  i += numChannels; j += 1
               }
               ch += 1
            }
            byteBuf.clear()
            byteBuf.put( arrayBuf, 0, m )
            byteBuf.flip()
            write.write( byteBuf )
            len -= chunkLen
            off += chunkLen
         }
      }
   }

   trait UByteLike extends BufferWriter {
      protected val arrayBuf : Array[ SByte ]

      protected def writeFrames( frames: Frames, off: Int, len: Int ) {
         while( len > 0 ) {
            val chunkLen   = min( frameBufCapacity, len )
            val m			   = chunkLen * bytesPerFrame
            var ch = 0; while( ch < numChannels ) {
               val b = frames( ch )
               var i = ch; var j = off; while( i < m ) {
                  arrayBuf( i ) = (b( j ) * 0x7F + 0x80).toByte
                  i += numChannels; j += 1
               }
               ch += 1
            }
            byteBuf.clear()
            byteBuf.put( arrayBuf, 0, m )
            byteBuf.flip()
            write.write( byteBuf )
            len -= chunkLen
            off += chunkLen
         }
      }
   }

   trait ShortLike extends BufferWriter {
      protected val viewBuf : ShortBuffer
      protected val arrayBuf : Array[ SShort ]

      protected def writeFrames( frames: Frames, off: Int, len: Int ) {
         while( len > 0 ) {
            val chunkLen   = min( frameBufCapacity, len )
            val m			   = chunkLen * numChannels
            var ch = 0; while( ch < numChannels ) {
               val b = frames( ch )
               if( b != null ) {
                  var i = ch; var j = off; while( i < m ) {
                     arrayBuf( i ) = (b( j ) * 0x7FFF).toShort
                     i += numChannels; j += 1
                  }
               }
               ch += 1
            }
            viewBuf.clear()
            viewBuf.put( arrayBuf, 0, m )
            byteBuf.rewind().limit( chunkLen * bytesPerFrame )
            write.write( byteBuf )
            len -= chunkLen
            off += chunkLen
         }
      }
   }

   trait ThreeByteLike extends BufferWriter {
      protected val arrayBuf : Array[ SShort ]
      protected val chStep : Int

      protected def writeFrames( frames: Frames, off: Int, len: Int ) {
         while( len > 0 ) {
            val chunkLen   = min( frameBufCapacity, len )
            val m			   = chunkLen * bytesPerFrame
            var ch = 0; while( ch < numChannels ) {
               val b = frames( ch )
               var i = ch * 3; var j = off; while( i < m ) {
                  val k = (b( j ) * 0x7FFFFF).toInt
                  arrayBuf( i )     = (k >> 16).toByte
                  arrayBuf( i + 1 ) = (k >> 8).toByte
                  arrayBuf( i + 2 ) = k.toByte
                  i += chStep; j += 1
               }
               ch += 1
            }
            byteBuf.clear()
            byteBuf.put( arrayBuf, 0, m )
            byteBuf.flip()
            write.write( byteBuf )
            len -= chunkLen
            off += chunkLen
         }
      }
   }

   trait ThreeLittleByteLike extends BufferWriter {
      protected val arrayBuf : Array[ SByte ]
      protected val chStep : Int

      protected def writeFrames( frames: Frames, off: Int, len: Int ) {
         while( len > 0 ) {
            val chunkLen   = min( frameBufCapacity, len )
            val m			   = chunkLen * bytesPerFrame
            var ch = 0; var p = 0; while( ch < numChannels ) {
               val b = frames( ch )
               if( b != null ) {
                  var i = p; j = off; while( i < m ) {
                     val k = (b( j ) * 0x7FFFFF).toInt;
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
            len -= chunkLen
            off += chunkLen
         }
      }
   }

   trait IntLike extends BufferWriter {
      protected val viewBuf : IntBuffer
      protected val arrayBuf : Array[ SInt ]

      protected def writeFrames( frames: Frames, off: SInt, len: SInt ) {
         while( len > 0 ) {
            val chunkLen   = min( frameBufCapacity, len )
            val m	         = chunkLen * numChannels
            var ch = 0; while( ch < numChannels ) {
               val b = frames( ch )
               if( b != null ) {
                  var i = ch; var j = off; while( i < m ) {
                     arrayBuf( i ) = (b( j ) * 0x7FFFFFFF).toInt
                     i += numChannels; j += 1
                  }
               }
               ch += 1
            }
            viewBuf.clear()
            viewBuf.put( arrayBuf, 0, m )
            byteBuf.rewind().limit( chunkLen * bytesPerFrame )
            write.write( byteBuf )
            len -= chunkLen
            off += chunkLen
         }
      }
   }

   trait FloatLike extends BufferWriter {
      protected val viewBuf : IntBuffer
      protected val arrayBuf : Array[ SFloat ]

      protected def writeFrames( frames: Frames, off: SInt, len: SInt ) {
         while( len > 0 ) {
            val chunkLen   = min( frameBufCapacity, len )
            val m			   = chunkLen * numChannels
            var ch = 0; while( ch < numChannels ) {
               val b = frames( ch )
               var i = ch; var j = off; while( i < m ) {
                  arrayBuf( i ) = b( j )
                  i += numChannels; j += 1
               }
               ch += 1
            }
            viewBuf.clear()
            viewBuf.put( arrayBuf, 0, m )
            byteBuf.rewind().limit( chunkLen * bytesPerFrame )
            write.write( byteBuf )
            len -=chunkLen
            off += chunkLen
         }
      }
   }

   trait DoubleLike extends BufferReader {
      protected val viewBuf : IntBuffer
      protected val arrayBuf : Array[ SDouble ]

      protected def writeFrames( frames: Frames, off: SInt, len: SInt ) {
         while( len > 0 ) {
            val chunkLen   = min( frameBufCapacity, len )
            val m			   = chunkLen * numChannels
            var ch = 0; while( ch < numChannels ) {
               val b = frames( ch )
               if( b != null ) {
                  var i = ch; var j = off; while( i < m ) {
                     arrayBuf( i ) = b( j )
                     i += numChannels; j += 1
                  }
               }
               ch += 1
            }
            viewBuf.clear()
            viewBuf.put( arrayBuf, 0, m );
            byteBuf.rewind().limit( chunkLen * bytesPerFrame )
            write.write( byteBuf )
            len -= chunkLen
            off += chunkLen
         }
      }
   }
}

// -------------- Handler -------------- 

object BufferHandler {
   class Byte( protected val write: ReadableByteChannel, protected val write: WritableByteChannel,
               protected val byteBuf: ByteBuffer )
   extends BufferReader.ByteLike with BufferWriter.ByteLike with BufferHandler {
      protected val arrayBuf = new Array[ SByte ]( byteBuf.capacity() )
   }

   class UByte( protected val write: ReadableByteChannel, protected val write: WritableByteChannel,
                protected val byteBuf: ByteBuffer )
   extends BufferReader.UByteLike with BufferWriter.UByteLike with BufferHandler {
      protected val arrayBuf = new Array[ SByte ]( byteBuf.capacity() )
   }

   class Short( protected val write: ReadableByteChannel, protected val write: WritableByteChannel,
                protected val byteBuf: ByteBuffer )
   extends BufferReader.ShortLike with BufferWriter.ShortLike with BufferHandler {
      byteBuf.clear
      protected val viewBuf	= byteBuf.asShortBuffer()
      protected val arrayBuf	= new Array[ SShort ]( viewBuf.capacity() )
   }

   class ThreeByte( protected val write: ReadableByteChannel, protected val write: WritableByteChannel,
                    protected val byteBuf: ByteBuffer )
   extends BufferReader.ThreeByteLike with BufferWriter.ThreeByteLike with BufferHandler {
      protected val arrayBuf = new Array[ Byte ]( byteBuf.capacity() )
      protected val chStep   = numChannels * 3
   }

   class ThreeLittleByte( protected val write: ReadableByteChannel, protected val write: WritableByteChannel,
                          protected val byteBuf: ByteBuffer )
   extends BufferReader.ThreeLittleByteLike with BufferWriter.ThreeLittleByteLike with BufferHandler {
      protected val arrayBuf = new Array[ Byte ]( byteBuf.capacity() )
      protected val chStep   = numChannels * 3
   }

   class Int( protected val write: ReadableByteChannel, protected val write: WritableByteChannel,
              protected val byteBuf: ByteBuffer )
   extends BufferReader.IntLike with BufferWriter.IntLike with BufferHandler {
      byteBuf.clear
      protected val viewBuf	= byteBuf.asIntBuffer()
      protected val arrayBuf	= new Array[ SInt ]( viewBuf.capacity() )
   }

   class Float( protected val write: ReadableByteChannel, protected val write: WritableByteChannel,
                protected val byteBuf: ByteBuffer )
   extends BufferReader.FloatLike with BufferWriter.FloatLike with BufferHandler {
      byteBuf.clear()
      protected val viewBuf	= byteBuf.asFloatBuffer()
      protected val arrayBuf	= new Array[ SFloat ]( viewBuf.capacity() )
   }

   class Double( protected val write: ReadableByteChannel, protected val write: WritableByteChannel,
                 protected val byteBuf: ByteBuffer )
   extends BufferReader.DoubleLike with BufferWriter.DoubleLike with BufferHandler {
      byteBuf.clear()
      protected val viewBuf	= byteBuf.asDoubleBuffer()
      protected val arrayBuf	= new Array[ SDouble ]( viewBuf.capacity() )
   }
}