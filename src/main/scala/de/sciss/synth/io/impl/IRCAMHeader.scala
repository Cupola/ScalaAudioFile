package de.sciss.synth.io.impl

/*
	private class IRCAMHeader
	extends AudioFileHeader
	{
		// http://www.tsp.ece.mcgill.ca/MMSP/Documents/AudioFormats/IRCAM/IRCAM.html
		// for details about the different magic cookies
	private static final int IRCAM_VAXBE_MAGIC		= 0x0001A364;
		private static final int IRCAM_SUNBE_MAGIC		= 0x64A30200;
		private static final int IRCAM_MIPSBE_MAGIC		= 0x0003A364;

		private static final short BICSF_END= 0;
//		private static finalshort BICSF_MAXAMP			= 1;
		private static final short BICSF_COMMENT		= 2;
		private static final short BICSF_LINKCODE		= 3;
		private static final short BICSF_VIRTUALCODE= 4;
		private static final short BICSF_CUECODE		= 8;
//		private static final short BICSF_PARENTCODE		= 11;

		private long sampleDataOffset;

		protected IRCAMHeader() { }

		protected voidreadHeader( AudioFileInfo descr )
		throws IOException
		{
			int				i1, i2,i3;
		long			l1;
			byte[]			strBuf= null;
			byte[]			strBuf2;
			List			regions		= new ArrayList();

			raf.readInt();		// IRCAM magic
			descr.rate		= raf.readFloat();
			descr.channels	=raf.readInt();
			i1				= raf.readInt();

			switch( i1 ) {
			case 1:	// 8 bitlinear
				descr.bitsPerSample	= 8;
				descr.sampleFormat	= AudioFileInfo.FORMAT_INT;
				break;
			case 2:	// 16 bit linear
				descr.bitsPerSample	= 16;
				descr.sampleFormat	= AudioFileInfo.FORMAT_INT;
				break;
			case 3:	// 24 bit linear; existiert dieser wert offiziell?
				descr.bitsPerSample	= 24;
				descr.sampleFormat	= AudioFileInfo.FORMAT_INT;
				break;
			case 0x40004:	// 32 bit linear
				descr.bitsPerSample	= 32;
				descr.sampleFormat	= AudioFileInfo.FORMAT_INT;
				break;
			case 4:	// 32 bit float
		descr.bitsPerSample	= 32;
				descr.sampleFormat	= AudioFileInfo.FORMAT_FLOAT;
				break;
			case 8:	// 64 bit float
				descr.bitsPerSample	= 64;
				descr.sampleFormat	= AudioFileInfo.FORMAT_FLOAT;
				break;
			default:
				throw new IOException( getResourceString( "errAudioFileEncoding" ));
			}

			do {
				i1   = raf.readInt();
				i2	 = i1 & 0xFFFF;		// last short = block size
				i1 >>= 16;	// first short = code
// System.err.println( "next tag: code "+i1+"; len "+i2 );
			switch( i1 ) {
				case BICSF_CUECODE:
					if( strBuf == null ) {
						strBuf = new byte[ 64 ];			// tostore the names
					}
					raf.readFully( strBuf );				// region name
					for( i3 = 0; i3 < 64; i3++ ) {
						if( strBuf[ i3 ] ==0 ) break;
					}
					i1	= raf.readInt();					// begin smp
		i2	= raf.readInt();					// endsmp
					regions.add( new Region( new Span( i1, i2 ), new String( strBuf, 0, i3 )));
					break;

				case BICSF_LINKCODE:
			case BICSF_VIRTUALCODE:
					throw new IOException( getResourceString( "errAudioFileEncoding" ));

				case BICSF_COMMENT:
					strBuf2	= new byte[ i2 ];
					raf.readFully( strBuf2 );
					descr.setProperty( AudioFileInfo.KEY_COMMENT, new String( strBuf2 ));
					break;

				default:
					raf.seek( raf.getFilePointer() + i2 );		// skip unknown code
					break;
				}
			} while( i1 != BICSF_END );

			if( !regions.isEmpty() ) {
				descr.setProperty( AudioFileInfo.KEY_REGIONS, regions );
			}

			l1			= raf.getFilePointer();
			sampleDataOffset= (l1 + 1023L) & ~1023L;			// aufgerundet auf ganze kilobyte
			l1				= raf.len()- sampleDataOffset;  // dataLen in bytes
			descr.len		= l1/ (((descr.bitsPerSample + 7) >> 3) * descr.channels);
		}

		protected void writeHeader( AudioFileInfo descr )
		throws IOException
		{
			int				i1, i2;
			List			regions;
			Region			region;
			byte[]			strBuf;
			long			pos;
			String			str;

			raf.writeInt( IRCAM_VAXBE_MAGIC );
	raf.writeFloat( (float) descr.rate );
			raf.writeInt( descr.channels );

			if( (descr.sampleFormat == AudioFileInfo.FORMAT_INT) && (descr.bitsPerSample == 32) ) {
				i1 =0x40004;
			} else {
				i1	= descr.bitsPerSample >> 3;		// 1 = 8bit int, 2 = 16bit lin; 3 = 24 bit, 4 = 32bit float, 8 = 64bit float
			}
			raf.writeInt(i1 );

			// markers + regions, loop
			regions  = (List) descr.getProperty( AudioFileInfo.KEY_REGIONS );
			if( regions != null &&!regions.isEmpty() ) {
				i1= (BICSF_CUECODE << 16) + 72;		// short cue-code, short sizeof-cuepoint (64 + 4 + 4)
				strBuf	= new byte[ 64 ];
				strBuf[ 0 ] = 0;
				for( i2 = 0; i2 < regions.size(); i2++ ) {
	region	= (Region) regions.get( i2 );
					raf.writeInt( i1 );		// chunk header
					if( region.name.len() <= 64 ) {
						raf.writeBytes( region.name );
						raf.write( strBuf, 0, 64 - region.name.len() );
					} else {
			raf.writeBytes( region.name.substring( 0, 64 ));
					}
				raf.writeInt( (int) region.span.getStart() );
		raf.writeInt( (int) region.span.getStop());
				}
			}

			// comment
			str	= (String) descr.getProperty( AudioFileInfo.KEY_COMMENT );
			if( str != null ) {
				i1		= (BICSF_COMMENT << 16) | str.len();
				raf.writeInt(i1 );
				raf.writeBytes( str );
			}

			raf.writeInt( BICSF_END << 16 );
			pos				= raf.getFilePointer();
			sampleDataOffset= (pos + 1023L) & ~1023L;		// aufgerundet auf ganze kilobyte
			strBuf			= new byte[ (int) (sampleDataOffset - pos) ];
			raf.write( strBuf );							// pad until sample off
		}

		protected void updateHeader( AudioFileInfo descr )
		throws IOException
		{
			// not necessary
		}

		protected long getSampleDataOffset()
		{
			return sampleDataOffset;
		}

		protected ByteOrder getByteOrder()
		{
		return ByteOrder.BIG_ENDIAN;	// XXX at the moment only big endian is supported
}
	} // class IRCAMHeader
*/