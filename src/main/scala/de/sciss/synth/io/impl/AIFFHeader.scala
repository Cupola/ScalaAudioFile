/*
 *  AIFFHeader.scala
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

package de.sciss.synth.io.impl

import de.sciss.synth.io._
import java.nio.ByteOrder
import ByteOrder.{ BIG_ENDIAN, LITTLE_ENDIAN }
import java.io.{ DataInput, DataInputStream, EOFException, IOException, RandomAccessFile }
import math._

object AIFFHeader extends AudioFileHeaderFactory {
   private val FORM_MAGIC		= 0x464F524D	// 'FORM'
   private val AIFF_MAGIC		= 0x41494646	// 'AIFF'   (off 8)
   private val AIFC_MAGIC		= 0x41494643	// 'AIFC'   (off 8)

   // chunk identifiers
   private val COMM_MAGIC		= 0x434F4D4D	// 'COMM'
   private val INST_MAGIC		= 0x494E5354	// 'INST'
   private val MARK_MAGIC		= 0x4D41524B	// 'MARK'
   private val SSND_MAGIC		= 0x53534E44	// 'SSND
   private val FVER_MAGIC		= 0x46564552	// 'FVER
   private val APPL_MAGIC		= 0x4150504C	// 'APPL'
   private val COMT_MAGIC		= 0x434F4D54	// 'COMT'
   private val ANNO_MAGIC		= 0x414E4E4F	// 'ANNO'

// aifc compression identifiers
   private val NONE_MAGIC		= 0x4E4F4E45	// 'NONE' (AIFC-compression)
   private val fl32_MAGIC		= 0x666C3332	// 'fl32' (AIFC-compression)
   private val FL32_MAGIC		= 0x464C3332	// SoundHack variant
   private val fl64_MAGIC		= 0x666C3634
   private val FL64_MAGIC		= 0x464C3634   // SoundHack variant
   private val in16_MAGIC		= 0x696E3136	// we "love" SoundHack for its special interpretations
   private val in24_MAGIC		= 0x696E3234
   private val in32_MAGIC		= 0x696E3332
   private val in16LE_MAGIC	= 0x736F7774	// 'sowt' (16-bit PCM little endian)

   private val AIFCVersion1	= 0xA2805140	// FVER chunk
// private val NONE_HUMAN	   = "uncompressed"
   private val fl32_HUMAN	   = "32-bit float"
   private val fl64_HUMAN	   = "64-bit float"

   // ---- AudioFileHeaderFactory ----
   def createHeaderReader : Option[ AudioFileHeaderReader ] = Some( new Reader )
   def createHeaderWriter : Option[ AudioFileHeaderWriter ] = None // XXX

   @throws( classOf[ IOException ])
   def identify( dis: DataInputStream ) = dis.readInt() match {
      case FORM_MAGIC => {	         // -------- probably AIFF --------
         dis.readInt()
         val magic = dis.readInt()
         magic == AIFC_MAGIC || magic == AIFF_MAGIC
      }
      case _ => false
   }

   private class Reader extends AudioFileHeaderReader {
      import AudioFileHeader._

      @throws( classOf[ IOException ])
      def read( raf: RandomAccessFile ) : AudioFileHeader = readDataInput( raf )

      @throws( classOf[ IOException ])
      def read( dis: DataInputStream ) : AudioFileHeader = readDataInput( dis )

      @throws( classOf[ IOException ])
      private def readDataInput( din: DataInput ) : AudioFileHeader = {
         if( din.readInt() != FORM_MAGIC ) formatError  // FORM
         // trust the file len more than 32 bit form field which
         // breaks for > 2 GB (> 1 GB if using signed ints)
         din.readInt()
//       var len           = dis.length() - 8
//			var len           = (dis.readInt() + 1).toLong & 0xFFFFFFFEL // this gives 32 bit unsigned space (4 GB)
         val isAIFC        = din.readInt() match {
            case AIFC_MAGIC   => true
            case AIFF_MAGIC   => false
            case m            => formatError
         }
//         len	           -= 4
         var chunkLen      = 0   // updated per chunk; after each chunk we skip the remaining bytes

         // these we need...
         var afh: AudioFileHeader   = null
         var ssndFound              = false

         try {
            while( !ssndFound ) {
               if( chunkLen != 0 ) din.skipBytes( chunkLen )  // skip remainder from previous chunk

               val magic   = din.readInt()
               chunkLen	   = (din.readInt() + 1) & 0xFFFFFFFE

               magic match {
               case COMM_MAGIC => { // reveals spec
                  val numChannels   = din.readShort()
//                commSmpNumOffset  = dis.getFilePointer()
                  val numFrames		= din.readInt().toLong & 0xFFFFFFFFL
                  val bitsPerSample = din.readShort()

                  // suckers never die. perhaps the most stupid data format to store a float:
                  val l1 				= din.readLong()
                  val l2	 			= din.readUnsignedShort()
                  val l3	 			= l1 & 0x0000FFFFFFFFFFFFL
                  val i1				= ((l1 >> 48).toInt & 0x7FFF) - 0x3FFE
                  val sampleRate    = ((l3 * pow( 2.0, i1 - 48 )) +
                                       (l2 * pow( 2.0, i1 - 64 ))) * signum( l1 )

                  chunkLen         -= 18
                  val (byteOrder, sampleFormat) = if( isAIFC ) {
                     chunkLen -= 4
                     din.readInt() match {
                        case NONE_MAGIC      => (BIG_ENDIAN, intSampleFormat( bitsPerSample ))
                        case `in16_MAGIC`    => (BIG_ENDIAN, SampleFormat.Int16)
                        case `in24_MAGIC`    => (BIG_ENDIAN, SampleFormat.Int24)
                        case `in32_MAGIC`    => (BIG_ENDIAN, SampleFormat.Int32)
                        case `fl32_MAGIC`    => (BIG_ENDIAN, SampleFormat.Float)
                        case FL32_MAGIC      => (BIG_ENDIAN, SampleFormat.Float)
                        case `fl64_MAGIC`    => (BIG_ENDIAN, SampleFormat.Double)
                        case FL64_MAGIC      => (BIG_ENDIAN, SampleFormat.Double)
                        case `in16LE_MAGIC`  => (LITTLE_ENDIAN, SampleFormat.Int16)
                        case m               => throw new IOException( "Unsupported AIFF encoding (" + m + ")" )
                     }
                  } else (BIG_ENDIAN, intSampleFormat( bitsPerSample ))

                  val spec = new AudioFileSpec( AudioFileType.AIFF, sampleFormat, numChannels, sampleRate, numFrames )
                  afh = new ReadableHeader( spec, byteOrder )
               }

               case INST_MAGIC => {
//               dis.readInt();	// char: MIDI Note, Detune, LowNote, HighNote
//               i1		= dis.readInt();		// char velocityLo, char velocityHi, short gain [dB]
//               descr.setProperty( AudioFileInfo.KEY_GAIN,
//                            new Float( Math.exp( (double) (i1 & 0xFFFF) / 20 * Math.log( 10 ))));
//               i1	 				= dis.readShort();// Sustain-Loop: 0 = no loop, 1 = fwd, 2 = back
//               loop				= i1 != 0;
//               i1					= dis.readInt();		// Short Lp-Start-MarkerID, Short End-ID
//               loopStart			= (i1 >> 16) & 0xFFFF;
//               loopEnd				=i1 & 0xFFFF;
//               chunkLen -= 14;
               }

               case MARK_MAGIC => {
//               markersOffset = dis.getFilePointer();		// read them out later
               }

               case SSND_MAGIC => {
                  val i1            = din.readInt() // sample data off
                  din.readInt()
                  din.skipBytes( i1 )
                  ssndFound         = true   // important: this must be the last case block statement coz we catch EOF!
               }

               case APPL_MAGIC => {
//               strBuf		= new byte[ 4 ];
//               dis.readFully( strBuf );		//App code
//               chunkLen   -= 4;
//               descr.appCode	= new String( strBuf );
//               appCodeOff	= dis.getFilePointer();
//               appCodeLen	= chunkLen;
               }

               case COMT_MAGIC => {
//               i1= dis.readShort();	// number of comments
//               chunkLen -= 2;
//   commentLp:		for( i = 0; !comment && (i < i1); i++ ) {
//                  dis.readInt();				// time stamp (ignore)
//               i2	= dis.readInt();		// markerID << 16 | count
//                  chunkLen -= 8;
//                  if( (i2 != 0) && ((i2 >> 16) == 0) ) {		// ok, not empty and not linked to a marker
//                     strBuf  = new byte[ i2 ];
//                     // NOTE: although it states "Pascal String" in AIFF.h
//         // all text documents describing the chunk assume a plain string
//                     //; PString wouldn't make sense anyway because we have
//                     // the dedicated count field. Logic Pro 6 writes a PString
//         // but leaves count at zero, so this won't get read...
//                     dis.readFully( strBuf );
//                     descr.setProperty( AudioFileInfo.KEY_COMMENT, new String( strBuf ));
//                  if( (i2 & 1) == 1 ) {
//                        i2++;
//                        dis.readByte();
//                     }
//                     chunkLen   -= i2;
//                     comment		= true;
//                     break commentLp;
//
//                  } else {
//                     i2		  = (i2 + 1) & 0xFFFE;
//                     chunkLen -= i2;
//                     dis.seek( dis.getFilePointer() + i2 );
//                  }
//               }
               }

               case ANNO_MAGIC => {
//               if( !comment ) {
//                  strBuf		= new byte[chunkLen ];
//                  dis.readFully(strBuf );
//                  descr.setProperty( AudioFileInfo.KEY_COMMENT, new String( strBuf ));
//                  chunkLen	= 0;
//                  comment		= true;
//               }
               }

               case _ => // ignore unknown chunks
               } // magic match
            } // essentials loop
         } catch { case e: EOFException => }

         if( afh == null ) throw new IOException( "AIFF header misses COMM chunk" )
         if( !ssndFound )  throw new IOException( "AIFF header misses SSND chunk")

         afh
      }

      /*
       *    WARNING: it is crucial to add the return type here
       *    (see scala ticket #3440)
       */
      private def intSampleFormat( bitsPerSample: Int ) : SampleFormat = bitsPerSample match {
         case 8   => SampleFormat.Int8
         case 16  => SampleFormat.Int16
         case 24  => SampleFormat.Int24
         case 32  => SampleFormat.Int32
         case v   => throw new IOException( "Unsupported AIFF encoding (" + v + " bits-per-sample)" )
      }
   }

   private case class ReadableHeader( spec: AudioFileSpec, byteOrder: ByteOrder )
   extends AudioFileHeader {
//      def createBufferReader( read: ReadableByteChannel, bufSize: Int ) : Option[ BufferReader ] =
//         spec.sampleFormat.readerFactory.map( _.apply( read, byt, spec.numChannels ))
   }
}

   /*
class AIFFHeader extends AudioFileHeaderReader with AudioFileHeaderWriter {
   import AIFFHeader._

   private var isAIFC		      = true			// default for writing files

   private var sampleDataOffset  = -1L

   private var formLengthOffset	= 4L
   private var commSmpNumOffset  = -1L
   private var ssndLengthOffset  = -1L
   private var lastUpdateLength  = 0L
   // WARNING: this will be queried in openAsWrite, therefore
   // a default is required!!
   private var byteOrderVar      = ByteOrder.BIG_ENDIAN

   private var appCodeLen        = -1
   private var appCodeOff		   = 0L
   private var markersOffset	   = 0L
   private var loop				   = false
   private var loopStart	      = 0
   private var loopEnd		      = 0

   @throws( classOf[ IOException ])
   def write( spec: AudioFileSpec ) {
      int				i1, i2;
      String		str;
      byte[]			strBuf;
      Object			o;
      Region			region;
      List			markers;
      Marker			marker;
      double			d1, d2;
      long			pos, pos2;
      boolean			lp;

      isAIFC= descr.sampleFormat == AudioFileInfo.FORMAT_FLOAT;	// floating point requires AIFC compression extension
      dis.writeInt( FORM_MAGIC );
      dis.writeInt( 0 );				// Laenge ohne FORM-Header (Dateilaenge minus8); unknown now
      dis.writeInt( isAIFC ? AIFC_MAGIC : AIFF_MAGIC );

      // FVER Chunk
      if( isAIFC ) {
         dis.writeInt( FVER_MAGIC );
dis.writeInt( 4 );
         dis.writeInt( AIFCVersion1 );
      }

      // COMM Chunk
      dis.writeInt( COMM_MAGIC );
      pos =dis.getFilePointer();
      dis.writeInt( 0 );				// not known yet
      dis.writeShort( descr.channels );
      commSmpNumOffset = dis.getFilePointer();
      dis.writeInt( 0 );			// updated later
      dis.writeShort( isAIFC ? 16 : descr.bitsPerSample );	// a quite strange convention ...

      // suckers never die.
      i2		= (descr.rate <0.0) ? 128 : 0;
d2		= Math.abs( descr.rate  );
      i1		= (int) (Math.log( d2 ) / Math.log( 2 ) + 16383.0) & 0xFFFF;
d1		= d2 * (1 << (0x401E-i1));	// Math.pow( 2.0,0x401E - i1 );
      dis.writeShort( (((i2 | (i1 >> 8)) & 0xFF) << 8) | (i1 & 0xFF) );
      dis.writeInt( (int) ((long) d1 & 0xFFFFFFFF) );
      dis.writeInt( (int) ((long) ((d1 % 1.0) * 4294967296.0) & 0xFFFFFFFF) );

      if( isAIFC ) {
         if( descr.bitsPerSample == 32 ) {
            str	= fl32_HUMAN;
            i1	= fl32_MAGIC;
         } else {
            str = fl64_HUMAN;
   i1	= fl64_MAGIC;
         }
         dis.writeInt( i1 );
         dis.writeByte( str.len() );
         dis.writeBytes( str );
         if( (str.len() & 1) == 0 ) {
            dis.writeByte( 0x00);
//				} else {
//					dis.writeShort( 0x0000 );
         }
      }
      // ...chunk len update...
      pos2 = dis.getFilePointer();
      dis.seek( pos );
      dis.writeInt( (int) (pos2 - pos - 4) );
      dis.seek( pos2 );

      // INST Chunk
      dis.writeInt( INST_MAGIC );
      dis.writeInt( 20 );

//			f1	= (float) (12 * Math.log( (double) stream.base / 440.0 ) / Constants.ln2);
//			i1	= (int) (f1 + 0.5f);
//			b1	= (byte) ((f1 - (float) i1) * 100.0f);
//			writeInt( (((i1 + 69) & 0xFF) << 24) | ((int) b1 << 16) | 0x007F );	// char: MIDI Note, Detune, LowNote, HighNote
      dis.writeInt( (69 << 24) | (0 << 16) | 0x007F );	// char: MIDI Note, Detune, LowNote, HighNote

      // XXX the gain information could be updated in updateHeader()
      o = descr.getProperty( AudioFileInfo.KEY_GAIN );
      if( o != null ) {
         i1	= (int) (20 * Math.log( ((Float) o).floatValue() ) / Math.log( 10) + 0.5);
      } else {
         i1= 0;
      }
      dis.writeInt( (0x007F << 16) | (i1 & 0xFFFF) );		// char velLo, char velHi, short gain [dB]

      region  = (Region) descr.getProperty( AudioFileInfo.KEY_LOOP );
      lp	= region != null;
      dis.writeShort( lp ? 1 : 0 );					// No loop vs. loop forward
dis.writeInt( lp ? 0x00010002 : 0 );			// Sustain-Loop Markers
      dis.writeShort( 0 );							// No release loop
      dis.writeInt( 0 );

      markers= (List) descr.getProperty( AudioFileInfo.KEY_MARKERS );
      if( markers == null ) markers = Collections.EMPTY_LIST;
      // MARK Chunk
      if( lp || !markers.isEmpty() ) {
         dis.writeInt( MARK_MAGIC );
         pos= dis.getFilePointer();
         dis.writeInt( 0 );				// not known yet
i1	= markers.size() + (lp? 2 : 0);
         dis.writeShort( i1 );
         i2	= 1;					// ascending marker ID
         if( lp ) {
            dis.writeShort( i2++ );						// loopstart ID
            dis.writeInt( (int) region.span.getStart() );	// sample off
            dis.writeLong( 0x06626567206C7000L );		// Pascal style String: "beg lp"
            dis.writeShort( i2++ );
            dis.writeInt((int) region.span.getStop() );
            dis.writeLong( 0x06656E64206C7000L );		// Pascal style String: "end lp"
         }
for( i1 = 0; i1 < markers.size(); i1++ ) {
dis.writeShort( i2++ );
            marker = (Marker) markers.get( i1 );
   dis.writeInt( (int) marker.pos );
//	dis.writeByte( (marker.name.len() + 1) & 0xFE );
            dis.writeByte( marker.name.len()  & 0xFF );
            dis.writeBytes( marker.name);
            if( (marker.name.len() & 1) == 0 ) {
               dis.writeByte( 0x00 );
//					} else {
//						dis.writeShort( 0x2000 );	// padding space + zero pad to even address
            }
         }
         // ...chunk len update...
         pos2 = dis.getFilePointer();
         dis.seek( pos );
         dis.writeInt( (int) (pos2 - pos - 4) );
      dis.seek( pos2 );
      }

      // COMT Chunk
      str = (String) descr.getProperty( AudioFileInfo.KEY_COMMENT );
      if( (str != null) && (str.len() > 0) ) {
         dis.writeInt( COMT_MAGIC );
dis.writeInt( (11 + str.len()) & ~1 );
         dis.writeShort( 1 );			// just one comment
         // time stamp "seconds since 1904"; this stupid idea dies around 2030
         // when 32bit unsigned will be overflowed

protected val SECONDS_FROM_1904_TO_1970 = 2021253247L

dis.writeInt( (int) (System.currentTimeMillis() + SECONDS_FROM_1904_TO_1970) );
         dis.writeShort( 0 );			// no marker association
         dis.writeShort( str.len() );// count
         dis.writeBytes( str );
         if( (str.len() & 1) == 1 ) {
            dis.writeByte( 0 );			// pad
         }
      }

      // APPL Chunk
      strBuf	= (byte[]) descr.getProperty( AudioFileInfo.KEY_APPCODE );
      if( (descr.appCode != null) && (strBuf != null) ){
         dis.writeInt( APPL_MAGIC );
         dis.writeInt( 4 + strBuf.len );
   dis.write( descr.appCode.getBytes(), 0, 4 );
      dis.write( strBuf );
         if( strBuf.len % 2 == 1 ) dis.write( 0 ); // pad
      }

      // SSND Chunk (Header)
      dis.writeInt( SSND_MAGIC );
      ssndLengthOffset = dis.getFilePointer();
      dis.writeInt( 8);		// + stream.samples * frameLength );
      dis.writeInt( 0 );		// sample
dis.writeInt( 0 );		// block size (?!)
      sampleDataOffset = dis.getFilePointer();

      updateHeader( descr );
   }

   @throws( classOf[ IOException ])
   def update( numFrames: Long ) {
      final long oldPos	= dis.getFilePointer();
      final long len		= dis.len();
      if( len == lastUpdateLength ) return;

      if( len >= formLengthOffset + 4 ) {
         dis.seek( formLengthOffset );
         dis.writeInt( (int) (len - 8) );								// FORM Chunk len
      }
      if( len >= commSmpNumOffset + 4 ) {
         dis.seek( commSmpNumOffset );
         dis.writeInt( (int) descr.len );								// COMM: Sample-Num
      }
      if( len >= ssndLengthOffset +4 ) {
         dis.seek( ssndLengthOffset );
         dis.writeInt( (int) (len - (ssndLengthOffset + 4)) );			// SSND Chunk len
      }
      dis.seek( oldPos );
      lastUpdateLength = len;
   }

//   protected longgetSampleDataOffset()
//   {
//      return sampleDataOffset;
//   }

   def byteOrder : ByteOrder = byteOrderVar

//   protected void readMarkers()
//   throws IOException
//   {
//      int i, i1, i2, i3;
//
//      if( markersOffset <= 0L ) return;
//
//      finalList		markers;
//      final byte[]	strBuf 		= new byte[ 64 ];	// to store the names
//      final long		oldPos		= dis.getFilePointer();
//      int				essentials	= loop ? 2 : 0; 	// start+end for sustain-loop
//
//      try {
//         dis.seek( markersOffset );
//         i1 = dis.readUnsignedShort();		// number of markers
//          markers = new ArrayList( i1);
//   for( i = i1; i > 0; i-- ) {
//   i3 =dis.readUnsignedShort();	// marker ID
//   i2 = dis.readInt();// marker position (sample off)
//            i1 = dis.readUnsignedByte();	// markerName String-len
//            if( loop && (i3 == loopStart) ) {
//               loopStart	= i2;
//               essentials--;
//            } else if( loop && (i3 == loopEnd) ) {
//            loopEnd		= i2;
//               essentials--;
//            } else {
//               i3	 = Math.min( i1, strBuf.len );
//               dis.readFully( strBuf,0, i3 );
//               i1	-= i3;
//               if( (i3 > 0) && (strBuf[ i3 - 1 ] == 0x20) ) {
//                  i3--;	// ignore padding space created by Peak
//               }
//               markers.add( new Marker( i2, new String( strBuf, 0, i3 )));
//            }
//            dis.seek( (dis.getFilePointer() + (i1 + 1)) & ~1 );
//         }
//         afd.setProperty( AudioFileInfo.KEY_MARKERS, markers );
//         if( loop && essentials ==0 ) {
//            afd.setProperty( AudioFileInfo.KEY_LOOP, new Region( new Span( loopStart, loopEnd ), NAME_LOOP ));
//         }
//      }
//      finally {
//         dis.seek( oldPos );
//      }
//   }

//   protected void readAppCode()
//   throws IOException
//   {
//      if( appCodeOff > 0 ) {
//         final byte[]	strBuf = new byte[ appCodeLen ];
//         final long		oldPos = dis.getFilePointer();
//dis.seek( appCodeOff );
//         dis.readFully( strBuf );
//         afd.setProperty( AudioFileInfo.KEY_APPCODE, strBuf );
//         dis.seek( oldPos );
//} else {
//         afd.setProperty( AudioFileInfo.KEY_APPCODE, null );
//      }
//   }
}
   */