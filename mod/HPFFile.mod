IMPLEMENTATION MODULE HPFFile;

FROM SYSTEM IMPORT
	ADDRESS, ADR;

FROM Storage IMPORT
	ALLOCATE, DEALLOCATE;

FROM RndFile IMPORT
	read, raw, old, FilePos, OpenResults,
	OpenOld, CurrentPos, StartPos, SetPos, NewPos, Close;

FROM IOResult IMPORT
	ReadResults, ReadResult;

FROM IOChan IMPORT
	RawRead;

FROM Strings IMPORT
	FindNext, Assign;

FROM WholeStr IMPORT
	ConvResults, StrToInt;

FROM LongConv IMPORT
	ValueReal;

FROM LongMath IMPORT
	round;


(*
	HPFReadChannelInformation - Read the Channel information chunk
	This chunk is an XML chunk which contains a description of the different
	signals and their units, data representation... etc.
*)
PROCEDURE HPFReadChannelInformation(VAR hpfFile : HPF_FILE) : BOOLEAN;
TYPE
	PTR_STRING = POINTER TO ARRAY OF CHAR;

VAR
	chunkPos : FilePos;
	lu : CARDINAL;
	xmlBufferSize : CARDINAL;
	ptrXmlBuffer : PTR_STRING;

	PROCEDURE ReadXMLInfo(VAR informationChunks : HPF_CHUNK; ptrXML : PTR_STRING);
	VAR
		i, k : CARDINAL;
		j : INTEGER;
		xmlTag : HPF_SET_CLAUSES_INFO;
		readValue : ARRAY[0..255] OF CHAR;
		found : BOOLEAN;
		sos : CHAR;
		res : ConvResults;

		PROCEDURE ReadXMLTag(CONST str : ARRAY OF CHAR;
			CONST tagsList : ARRAY OF CHAR) : HPF_SET_CLAUSES_INFO;
		VAR
			i, j : CARDINAL;
			tag : HPF_SET_CLAUSES_INFO;
			found : BOOLEAN;
		BEGIN
			FindNext(str, tagsList, 0, found, i);

			IF NOT found THEN
				RETURN invalid_tag
			END;

			tag := channel_information_data (* 0 *);

			FOR j := i TO 0 BY - 1 DO
				IF tagsList[j] = ',' THEN
					INC(tag)
				END
			END;

			RETURN tag
		END ReadXMLTag;

		PROCEDURE XmlValue(VAR dans : ARRAY OF CHAR);
		BEGIN
			FindNext("<", ptrXML^, 0, found, k);
			ptrXML^[k] := 0C;
			Assign(ptrXML^, dans);
			ptrXML^[k] := '<';
			ptrXML := ADR(ptrXML^[k])
		END XmlValue;
	BEGIN
		j := -1;

		LOOP
			FindNext(">", ptrXML^, 0, found, i);
			IF NOT found THEN EXIT END;
			INC(i);
			sos := ptrXML^[i];
			ptrXML^[i] := 0C;
			xmlTag := ReadXMLTag(ptrXML^, ch_clauses_info);
			ptrXML^[i] := sos;
			ptrXML := ADR(ptrXML^[i]);

			IF (xmlTag = invalid_tag) AND (xmlTag = channel_information_data) THEN
				CONTINUE
			ELSIF (xmlTag = channel_information) THEN
				INC(j);
				CONTINUE
			END;

			XmlValue(readValue);

			WITH informationChunks.channelData^[j] DO
				CASE xmlTag OF
					ch_name : Assign(readValue, name)
				|	internal_name : Assign(readValue, internalName)
				|	ch_color : (* NOTHING *)
				|	ch_unit :
						CASE readValue[0] OF
							'V' : unit := volt
						|	'g' : unit := g
						|	'd' : unit := deg_per_seg
						|	'u' : unit := u_tesla
						END;
				|	channel_type :
						CASE readValue[0] OF
							'R' : channelType := ch_random_data_channel
						|	'C' : channelType := ch_calculated_time_channel
						|	'M' : channelType := ch_monotonic_data_channel
						END;
				|	assigned_time_channel_index : StrToInt(readValue, assignedTimeChannelIndex, res)
				|	data_type :
						CASE readValue[0] OF
							'F' : dataType := ch_float
						|	'D' : dataType := ch_double
						|	'I' :
								IF readValue[3] = '1' THEN
									dataType := ch_int16
								ELSE
									dataType := ch_int32
								END
						END
				|	data_index : StrToInt(readValue, dataIndex, res)
				|	start_time : Assign(readValue, startTime)
				|	time_increment : timeIncrement := ValueReal(readValue)
				|	range_min : rangeMin := ValueReal(readValue)
				|	range_max : rangeMax := ValueReal(readValue)
				|	internal_gain : internalGain := ValueReal(readValue)
				|	internal_offset : internalOffset := ValueReal(readValue)
				|	external_gain : externalGain := ValueReal(readValue)
				|	external_offset : externalOffset := ValueReal(readValue)
				|	per_channel_sample_rate : perChannelSampleRate := ValueReal(readValue)
				|	requested_per_channel_sample_rate : requestedPerChannelSampleRate := ValueReal(readValue)
				|	per_channel_base_frequency_divider : perChannelBaseFrequencyDivider := ValueReal(readValue)
				|	physical_channel_number : StrToInt(readValue, physicalChannelNumber, res)
				|	system_channel_number : StrToInt(readValue, systemChannelNumber, res)
				|	uses_sensor_values : usesSensorValues := (readValue[0] = 'T')
				|	start_trigger : Assign(readValue, startTrigger)
				|	stop_trigger : Assign(readValue, stopTrigger)
				|	requested_number_of_samples : StrToInt(readValue, requestedNumberOfSamples, res)
				ELSE (* invalid tag! *)
				END (* CASE xmlTag *)
			END (* WITH informationChunks.channelData^[j] "*)
		END
	END ReadXMLInfo;
BEGIN
	chunkPos := StartPos(hpfFile.chanId);

	IF NOT HPFSeekToChunk(hpfFile, HPF_SET_CHUNK_ID{ch_channel_information}, chunkPos) THEN
		RETURN FALSE
	END;

	RawRead(hpfFile.chanId, ADR(hpfFile.channelInformation), 24, lu);
	xmlBufferSize := hpfFile.channelInformation.chunkSize - 24;
	ALLOCATE(ptrXmlBuffer:ADDRESS, xmlBufferSize);
	RawRead(hpfFile.chanId, ptrXmlBuffer:ADDRESS, xmlBufferSize, lu);

	NEW(hpfFile.channelInformation.channelData, hpfFile.channelInformation.numberOfChannels - 1);
	ReadXMLInfo(hpfFile.channelInformation, ptrXmlBuffer);
	DEALLOCATE(ptrXmlBuffer:ADDRESS, xmlBufferSize);

	RETURN TRUE
END HPFReadChannelInformation;


(*
	HPFSeekToChunk - Seek to the first chunk with an ID in the chunkIdSet set,
	the function set the postion in the file and return the position in startingPos
	Note: no need to manually set the file position after calling this function
*)
PROCEDURE HPFSeekToChunk(CONST hpfFile : HPF_FILE; chunkIdSet : HPF_SET_CHUNK_ID;
	VAR startingPos : FilePos) : BOOLEAN;
VAR
	chunkPos : FilePos;
	readChunk : HPF_CHUNK;
	lu : CARDINAL;
	p : POINTER TO CARDINAL;
BEGIN
	(* Chunks are aligned to 64kiB, each chunk position should be a multiple of 64k (10000h) *)
	p := ADR(startingPos);
	IF (p^ MOD 10000h) # 0 THEN
		p^ := p^ + (10000h - (p^ MOD 10000h)); (* align to 64kiB *)
	END;

	SetPos(hpfFile.chanId, startingPos);
	chunkPos := startingPos;

	LOOP
		RawRead(hpfFile.chanId, ADR(readChunk), 16, lu); (* read chunkId and chunkSize *)

		IF ReadResult(hpfFile.chanId) = endOfInput THEN
			EXIT
		END;

		IF readChunk.chunkId IN chunkIdSet THEN
			SetPos(hpfFile.chanId, chunkPos);
			startingPos := chunkPos;
			RETURN TRUE
		END;

		chunkPos := NewPos(hpfFile.chanId, 1, readChunk.chunkSize, chunkPos);
		SetPos(hpfFile.chanId, chunkPos)
	END;

	RETURN FALSE
END HPFSeekToChunk;


(*
	HPFReadIndex - Read the index chunk if any
*)
PROCEDURE HPFReadIndex(VAR hpfFile : HPF_FILE) : BOOLEAN;
VAR
	chunkPos : FilePos;
	lu : CARDINAL;
BEGIN
	chunkPos := StartPos(hpfFile.chanId);

	(* Find the index chunk and return the starting position in chunkPos file position, return if no index chunk *)
	IF NOT HPFSeekToChunk(hpfFile, HPF_SET_CHUNK_ID{ch_index}, chunkPos) THEN
		RETURN FALSE
	END;

	(* Read the index chunk *)
	RawRead(hpfFile.chanId, ADR(hpfFile.index), 24, lu);

	(* Allocate and read the events list *)
	NEW(hpfFile.index.event, hpfFile.index.indexCount - 1);
	RawRead(hpfFile.chanId, hpfFile.index.event, hpfFile.index.indexCount * 40, lu);

	(* Move the position to the beginning of the next chunk *)
	SetPos(hpfFile.chanId, NewPos(hpfFile.chanId, 1, hpfFile.index.chunkSize, chunkPos));

	RETURN TRUE
END HPFReadIndex;


(*
	HPFBuildIndex - Build an in-memory index chunk if the file is not indexed
	Note: we don't write the built index to the file!, we built it in-memory to be
	used later.
*)
PROCEDURE HPFBuildIndex(VAR hpfFile : HPF_FILE) : BOOLEAN;
VAR
	chunkPos : FilePos;
	lu, i : CARDINAL;
	startIdx : INTEGER64;
	readChunk : HPF_CHUNK;
	desc : CHANNEL_DESCRIPTOR;
BEGIN
	chunkPos := StartPos(hpfFile.chanId);
	hpfFile.index.chunkId := ch_index;
	hpfFile.index.chunkSize := -1;
	hpfFile.index.indexCount := 0;

	(* count data sections *)
	WHILE HPFSeekToChunk(hpfFile, HPF_SET_CHUNK_ID{ch_data}, chunkPos) DO
		INC(hpfFile.index.indexCount);
		RawRead(hpfFile.chanId, ADR(readChunk), 16, lu);
		SetPos(hpfFile.chanId, NewPos(hpfFile.chanId, 1, readChunk.chunkSize, chunkPos));
		chunkPos := CurrentPos(hpfFile.chanId)
	END;

	(* allocate memory for index.event *)
	NEW(hpfFile.index.event, hpfFile.index.indexCount - 1);

	(* fill data sections *)
	chunkPos := StartPos(hpfFile.chanId);
	startIdx := 0;
	i := 0;
	WHILE HPFSeekToChunk(hpfFile, HPF_SET_CHUNK_ID{ch_data}, chunkPos) DO
		RawRead(hpfFile.chanId, ADR(readChunk), 32, lu);
		RawRead(hpfFile.chanId, ADR(desc), 8, lu);

		hpfFile.index.event^[i].perChannelDataLengthInSamples := desc.length / 4;
		hpfFile.index.event^[i].chunkId := readChunk.chunkId;
		hpfFile.index.event^[i].groupId := readChunk.groupId;
		hpfFile.index.event^[i].dataStartIndex := readChunk.dataStartIndex;
		hpfFile.index.event^[i].fileOffset := chunkPos;

		chunkPos := NewPos(hpfFile.chanId, 1, readChunk.chunkSize, chunkPos);
		SetPos(hpfFile.chanId, chunkPos);
		INC(i)
	END;

	RETURN TRUE
END HPFBuildIndex;


(*
	HPFOpenFile - Open an HPF file, the file handler will be stored in hpfFile.chanId
*)
PROCEDURE HPFOpenFile(CONST fileName : ARRAY OF CHAR; VAR hpfFile : HPF_FILE) : BOOLEAN;
VAR
	res : OpenResults;
	lu : CARDINAL;
BEGIN
	OpenOld(hpfFile.chanId, fileName, read + raw + old, res);
	IF res # opened THEN RETURN FALSE END;

	RawRead(hpfFile.chanId, ADR(hpfFile.header), 36, lu);
	IF (lu # 36) OR (hpfFile.header.chunkId # ch_header) OR (hpfFile.header.fileVersion # 10001h) THEN
		Close(hpfFile.chanId);
		RETURN FALSE
	END;

	RETURN TRUE
END HPFOpenFile;


(*
	HPFOpenAndInit - Open and initialise the hpfFile, 
*)
PROCEDURE HPFOpenAndInit(CONST fileName : ARRAY OF CHAR; VAR hpfFile : HPF_FILE) : BOOLEAN;
BEGIN
	IF NOT HPFOpenFile(fileName, hpfFile) THEN
		RETURN FALSE
	END;

	IF NOT(HPFReadChannelInformation(hpfFile) AND HPFReadIndex(hpfFile)) THEN
		HPFCloseFile(hpfFile);
		RETURN FALSE
	END;

	NEW(hpfFile.data.channelDescriptor, hpfFile.channelInformation.numberOfChannels - 1);

	RETURN TRUE
END HPFOpenAndInit;


(*
	HPFCloseFile
*)
PROCEDURE HPFCloseFile(VAR hpfFile : HPF_FILE);
BEGIN
	Close(hpfFile.chanId);
	DISPOSE(hpfFile.index.event);
	DISPOSE(hpfFile.channelInformation.channelData);
	DISPOSE(hpfFile.data.channelDescriptor)
END HPFCloseFile;


(*
	HPFReadAtTime - Read data at a given time (in seconds), notice that the time starts
	always at zero (0), for each file we starts at zero.
	If you want to know the starting time of the experiment, you can get this information
	from the hpfFile.channelInformation chunk.

	Notice: the data is read on demande, so we aren't reading the whole file to memory,
	in each time you call this function, it will calculate the location of closest data
	sample to the given time, then it reads that information
*)
PROCEDURE HPFReadAtTime(hpfFile : HPF_FILE; time : LONGREAL;
	CONST sensors : HPF_ARR_CHANNEL_SENSORS) : HPF_ARR_VALUES;
VAR
	result : HPF_ARR_VALUES;
	channel : HPF_CHANNEL;
	sensor : HPF_SENSOR;
	i, maxNumOfSamples, timeSampleIdx : INTEGER64;
	dataIdx, valueOffset, lu : CARDINAL;
	ptrIndexEvent : POINTER TO CHUNK_INDEX;
	ptrChannelInfo : POINTER TO CHUNK_CHANNEL_INFORMATION;
	val : REAL;
	useless : BOOLEAN;
BEGIN
	maxNumOfSamples := hpfFile.index.event^[hpfFile.index.indexCount - 1].dataStartIndex +
		hpfFile.index.event^[hpfFile.index.indexCount - 1].perChannelDataLengthInSamples;

	FOR channel := MIN(HPF_CHANNEL) TO MAX(HPF_CHANNEL) DO
		FOR sensor := MIN(HPF_SENSOR) TO MAX(HPF_SENSOR) DO
			dataIdx := INT(channel) * 10 + INT(sensor);
			ptrChannelInfo := ADR(hpfFile.channelInformation.channelData^[dataIdx]);
			timeSampleIdx := round(time * ptrChannelInfo^.perChannelSampleRate);

			IF (sensor IN sensors[channel]) AND (timeSampleIdx < maxNumOfSamples) THEN
				FOR i := 0 TO hpfFile.index.indexCount - 1 DO
					IF (hpfFile.index.event^[i].chunkId = ch_data) AND
							(timeSampleIdx >= hpfFile.index.event^[i].dataStartIndex) AND
							(timeSampleIdx < hpfFile.index.event^[i].dataStartIndex + hpfFile.index.event^[i].perChannelDataLengthInSamples) THEN
						useless := HPFReadData(hpfFile, hpfFile.index.event^[i].fileOffset, hpfFile.data);
						ptrIndexEvent := ADR(hpfFile.index.event^[i]);
						BREAK
					END
				END;

				(* channel data offset relative to chunk position in file *)
				valueOffset := hpfFile.data.channelDescriptor^[dataIdx].offset;

				(* the value (float = 64bit) offset *)
				valueOffset := valueOffset + (4 * VAL(CARDINAL, timeSampleIdx - ptrIndexEvent^.dataStartIndex));

				SetPos(hpfFile.chanId, NewPos(hpfFile.chanId, 1, valueOffset, ptrIndexEvent^.fileOffset));
				RawRead(hpfFile.chanId, ADR(val), 4, lu);

				result[channel, sensor] := VAL(LONGREAL, val) * ptrChannelInfo^.internalGain + ptrChannelInfo^.internalOffset;
			ELSE
				result[channel, sensor] := 0.
			END
		END
	END;

	RETURN result
END HPFReadAtTime;


(*
	HPFReadData - Read the data chunk
*)
PROCEDURE HPFReadData(hpfFile : HPF_FILE; startingPos : FilePos; VAR outChunk : HPF_CHUNK) : BOOLEAN;
VAR
	useless : CARDINAL;
BEGIN
	IF NOT HPFSeekToChunk(hpfFile, HPF_SET_CHUNK_ID{ch_data}, startingPos) THEN
		RETURN FALSE
	END;

	RawRead(hpfFile.chanId, ADR(outChunk), 32, useless);

	IF outChunk.channelDataCount # hpfFile.channelInformation.numberOfChannels THEN
		RETURN FALSE
	END;

	RawRead(hpfFile.chanId, outChunk.channelDescriptor, outChunk.channelDataCount * 8, useless);

	RETURN TRUE
END HPFReadData;

END HPFFile.
