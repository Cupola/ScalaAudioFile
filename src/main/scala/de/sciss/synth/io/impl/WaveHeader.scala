package de.sciss.synth.io.impl

/*
	private abstract class AbstractRIFFHeader
	extends AudioFileHeader
	{
		protected static final int ADTL_MAGIC		=0x6164746C;	// 'adtl'
		protected static finalintLABL_MAGIC		= 0x6C61626C;	// 'labl'
		protected static final int LTXT_MAGIC		= 0x6C747874;	// 'ltxt'

		// ltxt purpose for regions
	protected static final int RGN_MAGIC		= 0x72676E20;	// 'rgn '

		// fmt format-code
		protected static final int FORMAT_PCM		= 0x0001;
		protected static final int FORMAT_FLOAT		= 0x0003;
		protected static final int FORMAT_EXT		= 0xFFFE;


		protected long 		sampleDataOffset;
		protected long		dataLengthOffset;
		protected long		factSmpNumOffset;
		protected long		lastUpdateLength	= 0L;
		protected boolean	isFloat	= false;
		protected boolean	unsignedPCM;

		protected AbstractRIFFHeader() {  }
	}

	// WAVE is the most stupid and chaotic format. there are dozens
	// of alternatives to say the same thing and i'm too lazy to apply higher
	// order heuristics to find out what the creator application was trying
	// to say.
	//
	// therefore we are simply imitating the behaviour of bias peak
	// in terms of marker storage : use LIST/adtl withlabl chunk for the marker
	// names and ltxt chunks for regions (purposefield == "rgn "). the loop
	// region is stored outside in the smpl chunk.
	//
	// in other words, if an application stores normal regions in the
	// smpl chunk we'll missthem. so what, blame m****s***
	//
	// http://www.borg.com/%7Ejglatt/tech/wave.htm for a discussion
	private class WAVEHeader
	extends AbstractRIFFHeader
	{
		private static final int RIFF_MAGIC		= 0x52494646;	// 'RIFF'
		private static final int WAVE_MAGIC		= 0x57415645;	// 'WAVE' (off 8)

// chunk identifiers
		private static final int FMT_MAGIC		= 0x666D7420;	// 'fmt '
		private static final int FACT_MAGIC		= 0x66616374;	// 'fact'
		private static final int DATA_MAGIC		= 0x64617461;	//'data'
		private static final int CUE_MAGIC		= 0x63756520;	// 'cue '
		private static final int SMPL_MAGIC		= 0x73616D6C;	// 'smpl'
	private static final int INST_MAGIC		= 0x696E7374;	// 'inst'

		// embedded LIST (peak speak) / list (rest of the universe speak) format
		private static final int LIST_MAGIC		= 0x6C697374;	// 'list'
		private static final int LIST_MAGIC2	= 0x4C495354;	// 'LIST'

		private long		smplMagicOff		= 0L;
		private long		listMagicOff		= 0L;
		private long		listMagicLen		= 0L;
		private long		cueMagicOff			= 0L;
		private static final long riffLengthOffset = 4L;

		protected WAVEHeader() {   }

		protected void readHeader( AudioFileInfo descr )
		throws IOException
		{
			int		i,i1, i2, i3, chunkLen, essentials, magic, dataLen = 0, bpf = 0;
		long	len;

			raf.readInt();		// RIFF
raf.readInt();
len	= raf.len() - 8;
//			len		= (readLittleInt() + 1) & 0xFFFFFFFE;		// Laenge ohne RIFF-Header (Dateilaenge minus 8)
			raf.readInt();		// WAVE
	len	   -= 4;
			chunkLen = 0;

for( essentials = 2; (len > 0) && (essentials > 0); ) {
				if( chunkLen != 0 ) raf.seek( raf.getFilePointer()+ chunkLen );	// skip to next chunk

				magic		= raf.readInt();
				chunkLen	= (readLittleInt() + 1) & 0xFFFFFFFE;
				len		   -= chunkLen + 8;

		switch( magic ) {
	case FMT_MAGIC:
					essentials--;
					i					= readLittleUShort();		// format
				descr.channels		= readLittleUShort();	// # of channels
					i1					= readLittleInt();			// sample rate (integer)
					descr.rate			= i1;
					i2					= readLittleInt();			// bytes per frame and second (=#chan * #bits/8* rate)
					bpf= readLittleUShort();		// bytes per frame (=#chan * #bits/8)
	descr.bitsPerSample	= readLittleUShort();		// # of bits per sample
					if( ((descr.bitsPerSample& 0x07) != 0) ||
						((descr.bitsPerSample >> 3) * descr.channels != bpf) ||
					((descr.bitsPerSample >>3) * descr.channels * i1 != i2) ) {

						throw new IOException( getResourceString( "errAudioFileEncoding" ));
					}
					unsignedPCM			= bpf == 1;

					chunkLen-= 16;

					switch( i ) {
					case FORMAT_PCM:
						descr.sampleFormat = AudioFileInfo.FORMAT_INT;
						break;
					case FORMAT_FLOAT:
						descr.sampleFormat = AudioFileInfo.FORMAT_FLOAT;
						break;
					case FORMAT_EXT:
						if( chunkLen < 24 ) throw new IOException( getResourceString( "errAudioFileIncomplete" ));
						i1 = readLittleUShort();	// extension size
						if( i1 < 22 ) throw new IOException( getResourceString( "errAudioFileIncomplete" ));
						i2 = readLittleUShort();	// #valid bits per sample
						raf.readInt();				// channel mask, ignore
i3 = readLittleUShort();	// GUID first two bytes
						if( (i2 !=descr.bitsPerSample)||
							((i3 != FORMAT_PCM) &&
							(i3 != FORMAT_FLOAT)) ) throw new IOException( getResourceString( "errAudioFileEncoding" ));
						descr.sampleFormat = i3 == FORMAT_PCM ? AudioFileInfo.FORMAT_INT: AudioFileInfo.FORMAT_FLOAT;
						chunkLen -= 10;
						break;
					default:
						throw new IOException( getResourceString( "errAudioFileEncoding" ));
					}
					break;

				case DATA_MAGIC:
					essentials--;
					sampleDataOffset	= raf.getFilePointer();
					dataLen				= chunkLen;
					break;

				case CUE_MAGIC:
					cueMagicOff	= raf.getFilePointer();
					break;

				case LIST_MAGIC:
				case LIST_MAGIC2:
					i	= raf.readInt();
					chunkLen -= 4;
		if( i == ADTL_MAGIC ) {
						listMagicOff = raf.getFilePointer();
						listMagicLen = chunkLen;
		} // if( i == ADTL_MAGIC )
					break;

				case SMPL_MAGIC:
					smplMagicOff = raf.getFilePointer() +28;
					break;

		case INST_MAGIC:
					raf.readShort();	// skip UnshiftedNode, FineTune
					i = raf.readByte();	// gain (dB)
					if( i != 0 ) descr.setProperty( AudioFileInfo.KEY_GAIN, new Float( Math.exp(
						(double) i / 20 * Math.log( 10 ))));
				chunkLen-= 3;
					break;

	default:
					break;
				} // switch( magic )
			} // for( essentials = 2; (len > 0) && (essentials > 0); )
			if( essentials > 0 ) throw new IOException( getResourceString("errAudioFileIncomplete" ));

			descr.len	= dataLen / bpf;
		}

protected void writeHeader( AudioFileInfo descr )
		throws IOException
		{
			int				i, i1, i2, i3;
//			Stringstr;
			Region		region;
			List			markers, regions;
			Marker			marker;
			long			pos, pos2;
	Object			o;

			isFloat = descr.sampleFormat == AudioFileInfo.FORMAT_FLOAT;	// floating point requires FACT extension
raf.writeInt( RIFF_MAGIC );
			raf.writeInt( 0 );				// Laenge ohne RIFF-Header (Dateilaenge minus 8); unknown now
			raf.writeInt( WAVE_MAGIC );

			// fmt Chunk
			raf.writeInt( FMT_MAGIC );
	writeLittleInt( isFloat ? 18 : 16 );	// FORMAT_FLOAT has extension of size 0
			writeLittleShort( isFloat ? FORMAT_FLOAT : FORMAT_PCM );
	writeLittleShort( descr.channels );
			i1 = (int) (descr.rate + 0.5);
			writeLittleInt( i1 );
			i2 = (descr.bitsPerSample>> 3) * descr.channels;
			writeLittleInt( i1 * i2 );
			writeLittleShort( i2 );
			writeLittleShort( descr.bitsPerSample );

			if( isFloat ) raf.writeShort( 0 );

			// fact Chunk
			if( isFloat ) {
				raf.writeInt( FACT_MAGIC );
				writeLittleInt( 4 );
				factSmpNumOffset = raf.getFilePointer();
				raf.writeInt( 0 );
		}

			// cue Chunk
			markers  = (List) descr.getProperty( AudioFileInfo.KEY_MARKERS );
			regions  = (List) descr.getProperty( AudioFileInfo.KEY_REGIONS );
			if( ((markers != null) && !markers.isEmpty()) || ((regions != null) && !regions.isEmpty())) {
				if( markers == null ) markers = Collections.EMPTY_LIST;
				if( regions == null ) regions = Collections.EMPTY_LIST;

				raf.writeInt( CUE_MAGIC );
				i2	= markers.size() + regions.size();
				writeLittleInt( 24 * i2 + 4 );
				writeLittleInt( i2 );
				for( i= 0, i1 = 1; i < markers.size(); i++, i1++ ) {
marker = (Marker) markers.get( i );
					writeLittleInt( i1 );
					writeLittleInt( i1 );
					raf.writeInt( DATA_MAGIC );
					raf.writeLong( 0 );	// ignore dwChunkStart, dwBlockStart
					writeLittleInt( (int) marker.pos );
				}
				for( i = 0; i < regions.size(); i++, i1++ ) {
					region = (Region) regions.get( i );
					writeLittleInt( i1 );
					writeLittleInt( i1 );
					raf.writeInt( DATA_MAGIC );
					raf.writeLong( 0 );	// ignore dwChunkStart, dwBlockStart
					writeLittleInt( (int) region.span.getStart() );
				}

				raf.writeInt( LIST_MAGIC );
				pos	= raf.getFilePointer();
				raf.writeInt( 0);
				raf.writeInt( ADTL_MAGIC );

				for( i = 0, i1= 1; i < markers.size(); i++, i1++ ) {
		marker	= (Marker) markers.get( i );
					i3		= marker.name.len() + 5;
					raf.writeInt( LABL_MAGIC );
					writeLittleInt( i3 );
					writeLittleInt( i1 );
					raf.writeBytes( marker.name );
				if( (i3 & 1) == 0 ) raf.writeByte( 0 ); else raf.writeShort( 0 );
				}

				for( i= 0; i < regions.size(); i++, i1++ ) {
					region	= (Region) regions.get( i );
					i3	= region.name.len() + 5;
					raf.writeInt( LABL_MAGIC );
				writeLittleInt( i3 );
					writeLittleInt( i1 );
				raf.writeBytes( region.name );
					if((i3 & 1) == 0 ) raf.writeByte( 0); else raf.writeShort(0 );
				}

				for( i = 0, i1 = markers.size() + 1; i < regions.size(); i++, i1++ ) {
region	= (Region) regions.get( i );
					raf.writeInt( LTXT_MAGIC );
					writeLittleInt( 21 );
					writeLittleInt( i1 );
					writeLittleInt( (int) region.span.getLength() );
					raf.writeInt( RGN_MAGIC);
					raf.writeLong( 0 );		// wCountry, wLanguage, wDialect, wCodePage
raf.writeShort( 0 );	// no name (already specified in 'labl' chunk (zero + pad)
				}

				// update 'list' chunk size
				pos2 = raf.getFilePointer();
				i	 = (int)(pos2 - pos - 4);
				if( (i & 1) == 1 ) {
					raf.write( 0 );	// padding byte
					pos2++;
				}
				raf.seek( pos );
				writeLittleInt( i );
				raf.seek( pos2 );

	} // if marker or region list not empty

			// smpl Chunk
			region  = (Region) descr.getProperty( AudioFileInfo.KEY_LOOP );
			if( region != null ){
				raf.writeInt( SMPL_MAGIC );
				writeLittleInt( 36 + 24 );
				raf.writeLong( 0 );		// dwManufacturer,dwProduct
				writeLittleInt( (int) (1.0e9 / descr.rate + 0.5) );	// dwSamplePeriod
				writeLittleInt( 69 );	// dwMIDIUnityNote
				raf.writeInt( 0 );		// dwMIDIPitchFraction
				raf.writeLong( 0 );		// dwSMPTEFormat, dwSMPTEOffset
				writeLittleInt( 1 );	// just one loop
				raf.writeInt( 0 );		// no additional chunkinformation

				writeLittleInt( 0 );	// loop gets ID 0
				writeLittleInt( 0 );	// normal loop
	writeLittleInt( (int) region.span.getStart() );
				writeLittleInt( (int) region.span.getStop() );
				raf.writeLong( 0 );		// dwFraction, dwPlayCount
			}

		// inst Chunk
			o = descr.getProperty( AudioFileInfo.KEY_GAIN );
			if( o != null ) {
				i1	= Math.max( -64, Math.min( 63, (int) (20 * Math.log( ((Float) o).floatValue() ) / Math.log( 10 ) + 0.5) ));
				raf.writeInt( INST_MAGIC );
				writeLittleInt( 7 );
			raf.writeShort( (69 << 24) | (0 << 16) );	//char: MIDI Note, Detune
				raf.write( i1 );							// char gain (dB)
				raf.writeInt( 0x007F007F );					// char LowNote, HighNote, velLo, char velHi
				raf.write( 0 );								// pad byte
			}

			// data Chunk (Header)
			raf.writeInt( DATA_MAGIC );
			dataLengthOffset = raf.getFilePointer();
raf.writeInt( 0 );
			sampleDataOffset = raf.getFilePointer();

			updateHeader( descr );
		}

		protected void updateHeader( AudioFileInfo descr )
		throws IOException
		{
long oldPos	= raf.getFilePointer();
			long len	= raf.len();
			if( len == lastUpdateLength ) return;

			if( len >= riffLengthOffset + 4 ) {
				raf.seek( riffLengthOffset );
				writeLittleInt( (int) (len - 8) );								// RIFF Chunk len
			}
			if( isFloat &&(len >= factSmpNumOffset + 4) ) {
				raf.seek( factSmpNumOffset );
				writeLittleInt( (int) (descr.len * descr.channels) );			// fact: Sample-Num XXX check multich.!
			}
			if( len >= dataLengthOffset + 4 ) {
				raf.seek(dataLengthOffset );
				writeLittleInt( (int) (len - (dataLengthOffset + 4)) );			// data Chunk len
			}
			raf.seek( oldPos );
			lastUpdateLength= len;
		}

		protected long getSampleDataOffset()
{
			return sampleDataOffset;
		}

		protected ByteOrder getByteOrder()
		{
			return ByteOrder.LITTLE_ENDIAN;
		}

		protected boolean isUnsignedPCM()
		{
			return unsignedPCM;
		}

		protected void readMarkers()
		throws IOException
		{
			if( (smplMagicOff == 0L) && (listMagicOff == 0L) ) return;

			final Map	mapCues= new HashMap();
			final Map	mapCueLengths	= new HashMap();
			final Map	mapCueNames		= new HashMap();
			final long	oldPos			= raf.getFilePointer();
			final List	markers, regions;
		int			i, i1, i2, i3, i4, i5;
			Object		o;
			String		str;
			byte[]		strBuf			= null;

			try {
				if( smplMagicOff> 0L ) {
					raf.seek(smplMagicOff );
					i		  =readLittleInt();	// cSampleLoops
					raf.readInt();					// chunk extension len
		//			chunkLen -= 36;
					if( i > 0 ){
			i1	= readLittleInt(); 	// dwIdentifier
						o	= new Integer( i1 );
						mapCues.remove( o );
						mapCueLengths.remove( o );
						str	= (String) mapCueNames.remove( o );
						if( str == null ) str = NAME_LOOP;
						afd.setProperty( AudioFileInfo.KEY_LOOP, new Region( new Span(
							readLittleInt(), readLittleInt() ), str ));
		//				chunkLen -= 16;
					}
				}
				if( listMagicOff > 0L ) {
		raf.seek( listMagicOff );
					for( long chunkLen = listMagicLen; chunkLen >= 8; ) {
						i	=raf.readInt();		// sub chunkID
						i1	= readLittleInt();
						i2	= (i1 + 1) & 0xFFFFFFFE;	// subchunk len
						chunkLen -= 8;
						switch( i ) {
						case LABL_MAGIC:
							i3		  = readLittleInt();	// dwIdentifier
							i1		 -= 4;
							i2	     -= 4;
							chunkLen -=4;
							if( strBuf == null || strBuf.len < i1 ) {
						strBuf  = new byte[ Math.max( 64, i1 )];
							}
							raf.readFully( strBuf, 0, i1 );	// null-terminated
							mapCueNames.put( new Integer( i3 ), new String( strBuf, 0, i1 - 1 ));
						chunkLen -= i1;
							i2		 -= i1;
							break;

						case LTXT_MAGIC:
				i3			= readLittleInt();	// dwIdentifier
							i4			= readLittleInt();	// dwSampleLength (= frames)
							i5			= raf.readInt();	// dwPurpose
							raf.readLong();					// skip wCountry, wLanguage, wDialect, wCodePage
							i1			-= 20;
	i2			-= 20;
							chunkLen	-= 20;
							o = new Integer( i3 );
			if( (i1 > 0) && !mapCueNames.containsKey( o )) {// don't overwritenames
								if( strBuf == null || strBuf.len < i1 ) {
									strBuf  = new byte[ Math.max( 64, i1 )];
								}
								raf.readFully( strBuf, 0, i1 );	// null-terminated
								mapCueNames.put( o, new String( strBuf, 0, i1 - 1 ));
								chunkLen -= i1;
								i2		 -= i1;
		}
							if( (i4 > 0) || (i5 == RGN_MAGIC) ){
								mapCueLengths.put( o, new Integer( i4 ));
							}
							break;

						default:
							break;
						}
						if( i2 != 0 ) {
							raf.seek( raf.getFilePointer() + i2 );
chunkLen -= i2;
						}
					} // while( chunkLen >= 8 )
				}

				if( cueMagicOff > 0L ) {
raf.seek( cueMagicOff );
					i	= readLittleInt();	// num cues
					for( int j = 0; j < i; j++ ) {
						i1	= readLittleInt();	// dwIdentifier
						raf.readInt();			// dwPosition (ignore, we don't use playlist)
				i2	= raf.readInt();	// should be 'data'
						raf.readLong();			// ignore dwChunkStart and dwBlockStart
						i3	= readLittleInt();	// dwSampleOffset (fails for 64bit space)
	if( i2 == DATA_MAGIC ) {
				mapCues.put( new Integer( i1 ), new Integer( i3 ));
						}
					}
	//	chunkLen -= i * 24 + 4;
				}

				// resolve markers and regions
				if(!mapCues.isEmpty() ) {
				markers = new ArrayList();
					regions	= new ArrayList();
					for( Iterator iter = mapCues.keySet().iterator(); iter.hasNext(); ) {
						o	= iter.next();
		i	= ((Integer) mapCues.get( o )).intValue();	// start frame
				str	= (String) mapCueNames.get( o );
						o	= mapCueLengths.get( o );
					if( o == null ) {	// i.e. marker
if( str == null ) str = NAME_MARK;
							markers.add( new Marker( i, str ));
						} else {			// i.e. region
							if( str == null ) str = NAME_REGION;
							regions.add( new Region(new Span( i, ((Integer) o).intValue() ), str ));
						}
					}
					if( !markers.isEmpty() ) afd.setProperty( AudioFileInfo.KEY_MARKERS, markers );
					if( !regions.isEmpty() ) afd.setProperty( AudioFileInfo.KEY_REGIONS, regions );
				}
			}
finally {
				raf.seek( oldPos );
			}
		}
	} // class WAVEHeader

	// http://www.vcs.de/fileadmin/user_upload/MBS/PDF/Whitepaper/Informations_about_Sony_Wave64.pdf
	private class Wave64Header
	extends AbstractRIFFHeader
	{
		private static final int RIFF_MAGIC1a	= 0x72696666;	// 'riff'
		private staticfinal int RIFF_MAGIC1b	= 0x2E91CF11;
		private static final long RIFF_MAGIC1	= 0x726966662E91CF11L;
		private static final long RIFF_MAGIC2	= 0xA5D628DB04C10000L;

		private static final long WAVE_MAGIC1	= 0x77617665F3ACD311L;	// 'wave'-XXXX
		private static final long WAVE_MAGIC2	= 0x8CD100C04F8EDB8AL;

		// chunk identifiers
private static final long FMT_MAGIC1	= 0x666D7420F3ACD311L;	// 'fmt '-XXXX
		private static final long FMT_MAGIC2	= 0x8CD100C04F8EDB8AL;
		private static final long FACT_MAGIC1	= 0x66616374F3ACD311L;	// 'fact'-XXXX
		private static final long FACT_MAGIC2	= 0x8CD100C04F8EDB8AL;
		private static final long DATA_MAGIC1	= 0x64617461F3ACD311L;	// 'data'-XXXX
		private static final long DATA_MAGIC2	= 0x8CD100C04F8EDB8AL;

//		private static final long LIST_MAGIC1	= 0x6C6973742F91CF11L; // 'list'-XXXX
//		private static final longLIST_MAGIC2	= 0xA5D628DB04C10000L;
		private static final long MARKER_MAGIC1	= 0x5662F7AB2D39D211L;
		private static final long MARKER_MAGIC2	= 0x86C700C04F8EDB8AL;

		private final Charset charset = Charset.forName( "UTF-16LE" );
		private long		markersOffset			= 0L;
		private static final long riffLengthOffset	= 16L;

		protected Wave64Header() {  }

		protected void readHeader( AudioFileInfo descr )
		throws IOException
		{
			int		i, i1, i2, i3, essentials, bpf = 0;
			long	len, magic1, magic2, chunkLen, dataLen = 0;

			raf.readLong(); raf.readLong();		// riff
			len	= readLittleLong();
			raf.readLong(); raf.readLong();// wave
			len	   -= 40;
			chunkLen = 0;

//System.out.println( "len = " + len );

			for( essentials = 2; (len >= 24) && (essentials > 0); ){
				if( chunkLen != 0 ) raf.seek( raf.getFilePointer() + chunkLen );	// skip to next chunk

				magic1		= raf.readLong();
magic2		= raf.readLong();
				chunkLen	= (readLittleLong() + 7) & 0xFFFFFFFFFFFFFFF8L;

//System.out.println( "magic1 = " + magic1 +"; chunkLen = " + chunkLen + "; pos = " + raf.getFilePointer() );

				len		   -= chunkLen;
				chunkLen   -= 24;

				if( magic1 == FMT_MAGIC1 && magic2 == FMT_MAGIC2 ) {
					essentials--;
					i					= readLittleUShort();		// format
					descr.channels	= readLittleUShort();		// # of channels
					i1= readLittleInt();			// sample rate (integer)
	descr.rate			= i1;
					i2	= readLittleInt();			// bytes per frame and second (=#chan * #bits/8 * rate)
	bpf		= readLittleUShort();		// bytes per frame(=#chan * #bits/8)
					descr.bitsPerSample	= readLittleUShort();		// # of bits per sample
					if( ((descr.bitsPerSample & 0x07) != 0) ||
						((descr.bitsPerSample >> 3) * descr.channels != bpf) ||
						((descr.bitsPerSample >> 3) * descr.channels * i1 != i2) ) {

						throw new IOException( getResourceString( "errAudioFileEncoding" ));
					}
					unsignedPCM			= bpf == 1;

					chunkLen -=16;

					switch( i ) {
			case FORMAT_PCM:
		descr.sampleFormat = AudioFileInfo.FORMAT_INT;
						break;
					case FORMAT_FLOAT:
						descr.sampleFormat = AudioFileInfo.FORMAT_FLOAT;
						break;
					case FORMAT_EXT:
						if( chunkLen < 24 ) throw new IOException( getResourceString( "errAudioFileIncomplete" ));
						i1 = readLittleUShort();	// extension size
						if( i1 < 22 ) throw new IOException( getResourceString( "errAudioFileIncomplete" ));
						i2 = readLittleUShort();	// # valid bits per sample
						raf.readInt();				// channel mask, ignore
						i3 = readLittleUShort();	// GUID first two bytes
						if( (i2 != descr.bitsPerSample) ||
							((i3 != FORMAT_PCM) &&
							(i3 != FORMAT_FLOAT)) ) throw new IOException( getResourceString( "errAudioFileEncoding" ));
						descr.sampleFormat =i3 == FORMAT_PCM ? AudioFileInfo.FORMAT_INT : AudioFileInfo.FORMAT_FLOAT;
						chunkLen -= 10;
						break;
					default:
						throw new IOException( getResourceString( "errAudioFileEncoding" ));
					}

				} else if( magic1 == DATA_MAGIC1 && magic2 == DATA_MAGIC2 ) {
					essentials--;
					sampleDataOffset	= raf.getFilePointer();
					dataLen				= chunkLen;

				} else if( magic1 == MARKER_MAGIC1 && magic2 == MARKER_MAGIC2 ) {
					markersOffset			= raf.getFilePointer();
				}
			} // for( essentials = 2; (len > 0) && (essentials > 0); )
			if( essentials > 0 ) throw new IOException( getResourceString( "errAudioFileIncomplete" ));

			descr.len = dataLen / bpf;
		}

		protected void writeHeader( AudioFileInfo descr)
		throws IOException
		{
			final List		markers, regions;
			int				i1, i2;
			String			str;
			Region			region;
			Marker			marker;
			long			pos, pos2, n1, n2;

			isFloat = descr.sampleFormat == AudioFileInfo.FORMAT_FLOAT;	// floating point requires FACT extension
			raf.writeLong( RIFF_MAGIC1 );
			raf.writeLong( RIFF_MAGIC2 );
			raf.writeLong( 40 );		// Laenge inkl. RIFF-Header (Dateilaenge); unknown now
			raf.writeLong( WAVE_MAGIC1 );
			raf.writeLong( WAVE_MAGIC2 );

			// ---- fmt Chunk----
			raf.writeLong( FMT_MAGIC1 );
			raf.writeLong( FMT_MAGIC2 );
	writeLittleLong( isFloat ? 42 : 40 );  // FORMAT_FLOAT has extension of size 0
			writeLittleShort( isFloat ? FORMAT_FLOAT : FORMAT_PCM );
			writeLittleShort( descr.channels );
			i1 = (int) (descr.rate + 0.5);
		writeLittleInt( i1 );
			i2 = (descr.bitsPerSample >> 3) * descr.channels;
			writeLittleInt( i1 * i2 );
			writeLittleShort( i2 );
			writeLittleShort( descr.bitsPerSample );

			if( isFloat ) {
//raf.writeShort( 0 );
	raf.writeLong( 0 ); // actually a short, but six extra bytes to align to 8-byte boundary
			}

			// ---- fact Chunk ----
			if( isFloat ) {
				raf.writeLong( FACT_MAGIC1 );
				raf.writeLong( FACT_MAGIC2 );
				writeLittleLong( 32 );
				factSmpNumOffset = raf.getFilePointer();
//			raf.writeInt( 0 );
				raf.writeLong( 0 ); // i guess it shouldbe long???
			}

			// -- marker Chunk ----
			markers  = (List) descr.getProperty( AudioFileInfo.KEY_MARKERS );
			regions  = (List) descr.getProperty( AudioFileInfo.KEY_REGIONS );
			if( ((markers != null) && !markers.isEmpty()) || ((regions != null) && !regions.isEmpty()) ) {

				final CharsetEncoder	enc		= charset.newEncoder();
				CharBuffer				cbuf	= null;
				ByteBuffer				bbuf	= null;
				final List[] cues = {
					markers == null ? Collections.EMPTY_LIST : markers,
				regions == null? Collections.EMPTY_LIST : regions
				};

				raf.writeLong( MARKER_MAGIC1 );
				raf.writeLong( MARKER_MAGIC2 );
		pos	= raf.getFilePointer();
				raf.writeLong( 0 );// updated afterwards
				i2	= cues[ 0 ].size() + cues[ 1 ].size();
			writeLittleInt( i2 );
	// CUE64 structures
				for( int i = 0, id = 1; i < 2; i++ ) {
					for( int j = 0; j < cues[ i ].size(); j++, id++ ) {
						if( i == 0) {
							marker	= (Marker) cues[ i ].get( j );
							n1		= marker.pos;
							n2		= -1;
str		= marker.name;
						} else {
							region = (Region) cues[ i ].get( j);
							n1		= region.span.start;
							n2		= region.span.getLength();
							str		= region.name;
		}
						writeLittleInt( id );	// marker ID
						raf.writeInt( 0 );		// padding
						writeLittleLong( n1 );	// position
						writeLittleLong(n2 );	// len

						if( (cbuf == null) || (cbuf.capacity() < str.len()) ) {
							cbuf = CharBuffer.allocate( str.len() + 8 );
							bbuf = ByteBuffer.allocate( (cbuf.capacity() + 1) << 1 );
						}
						cbuf.clear();
						cbuf.put( str );
						cbuf.flip();
						bbuf.clear();
						enc.reset();
	enc.encode( cbuf, bbuf, true );
						enc.flush( bbuf);
						bbuf.putShort( (short) 0 ); // null term
						bbuf.flip();

						writeLittleInt( bbuf.remaining() );
		raf.writeInt( 0 );		// padding
//System.out.println( "writing " + bbuf.remaining() + " bytes at " + fch.position() );
						fch.write( bbuf );
					}
				}

				// update chunk size
				pos2 = raf.getFilePointer();
				n1	 = pos2 - pos;
//System.out.println( "n1 = " +n1 + "; pos = " + pos + "; pos2 = " + pos2 + "; pad = " + (int) (((n1 + 7) & 0xFFFFFFFFFFFFFFF8L) - n1) );
				final int pad = (int) (((n1 + 7) & 0xFFFFFFFFFFFFFFF8L)- n1);
			for( int i = 0; i < pad; i++ ) raf.write( 0 );	// padding byte

				raf.seek( pos );
				writeLittleLong( n1 + 16);
//				writeLittleLong( n1 + 16 + pad );
				raf.seek( pos2 + pad );

			} // if marker orregion list not empty

		// data Chunk (Header)
			raf.writeLong( DATA_MAGIC1 );
		raf.writeLong( DATA_MAGIC2 );
			dataLengthOffset = raf.getFilePointer();
			raf.writeLong( 24 );
			sampleDataOffset = raf.getFilePointer();

			updateHeader( descr );
		}

		protected void updateHeader( AudioFileInfo descr )
		throws IOException
		{
			final long oldPos	= raf.getFilePointer();
			final long len		= raf.len();
	if( len == lastUpdateLength ) return;
			final long lenM8	= len- 8;

			if( lenM8 >= riffLengthOffset ) {
				raf.seek( riffLengthOffset );
// System.out.println( "updateHeader: len = " + len );
				writeLittleLong( len );		// riff Chunk len
			}
			if( isFloat && (lenM8 >= factSmpNumOffset) ) {
				raf.seek( factSmpNumOffset );
				writeLittleLong( descr.len * descr.channels);			// fact: Sample-Num XXX check multich.!
			}
			if( lenM8 >= dataLengthOffset ) {
				raf.seek(dataLengthOffset );
				writeLittleLong( len - (dataLengthOffset - 16) );	// data Chunk len
			}
			raf.seek( oldPos );
			lastUpdateLength =len;
		}

		protected long getSampleDataOffset()
		{
			return sampleDataOffset;
		}

		protected ByteOrder getByteOrder()
		{
			return ByteOrder.LITTLE_ENDIAN;
		}

		protected boolean isUnsignedPCM()
		{
			return unsignedPCM;
		}

		protected void readMarkers()
throws IOException
		{
//System.out.println( "markersOffset = " + markersOffset );
			if( markersOffset == 0L ) return;

			final List				markers	= new ArrayList();
			final List	regions	= new ArrayList();
			final CharsetDecoder	dec		= charset.newDecoder();
			CharBuffer				cbuf	=null;
			ByteBuffer				bbuf	= null;
			long					n1, n2;
			int						numBytes;
			String		str;
			CoderResult				result;

			final long oldPos = raf.getFilePointer();
			try {
				raf.seek( markersOffset );
				for( int numCues = readLittleInt(), cue = 0; cue< numCues; cue++ ) {
//System.out.println( "cue " + (cue+1) + " of " + numCues );
					raf.readInt();					// marker ID (ignore)
		raf.readInt(); 					// padding
					n1			= readLittleLong();	// pos
					n2		= readLittleLong();	// len (-1 for markers)
					numBytes	= readLittleInt();	// size of name string in bytes
					raf.readInt(); 					// padding

			if( bbuf == null || bbuf.capacity() < numBytes ) {
			bbuf = ByteBuffer.allocate( numBytes + 16 );
						cbuf = CharBuffer.allocate( bbuf.capacity() >> 1 );
			}

					bbuf.rewind().limit( numBytes);

//System.out.println( "reading " + bbuf.remaining() + " bytes from " + fch.position() );

					fch.read( bbuf );
				if( (numBytes >= 2) &&
	    (bbuf.get( numBytes - 2 ) == 0) &&
   (bbuf.get( numBytes -1 ) == 0) ) { // null term

						bbuf.rewind().limit( numBytes - 2 );
			} else {
						bbuf.flip();
					}
	cbuf.clear();
					dec.reset();
					result = dec.decode( bbuf, cbuf, true );
					if( result.isError() ) {
						throw new IOException( "Error Reading Cue Name" +
						   (result.isMalformed() ? ": Malformed Input" :
					   (result.isOverflow() ? ": Overflow" :
						   (result.isUnderflow() ? ": Underflow" :
						   (result.isUnmappable() ? ": Unmappable" : "")))));
}
					dec.flush( cbuf );
					cbuf.flip();
		str = cbuf.toString();

// System.out.println( "n1 = " + n1 + "; n2 = " + n2 + "; name  = '" + str + "'" );

					if( n2 < 0 ) {
						markers.add( new Marker( n1, str ));
					} else {
						regions.add( new Region( new Span( n1, n1 + n2 ), str ));
					}
				}

				afd.setProperty( AudioFileInfo.KEY_MARKERS, markers );
				afd.setProperty( AudioFileInfo.KEY_REGIONS, regions );

			}
			finally {
				raf.seek( oldPos );
			}
		}
	} // class Wave64Header
*/