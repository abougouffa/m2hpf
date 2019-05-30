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


(* HPFReadChannelInformation *)
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
		tagLu : HPF_SET_CLAUSES_INFO;
		valeurLue : ARRAY[0..255] OF CHAR;
		trouve : BOOLEAN;
		sos : CHAR;
		res : ConvResults;

		PROCEDURE ReadXMLTag(CONST str : ARRAY OF CHAR;
			CONST tagsList : ARRAY OF CHAR) : HPF_SET_CLAUSES_INFO;
		VAR
			i, j : CARDINAL;
			tag : HPF_SET_CLAUSES_INFO;
			trouve : BOOLEAN;
		BEGIN
			FindNext(str, tagsList, 0, trouve, i);

			IF NOT trouve THEN
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

		PROCEDURE ValeurXml(VAR dans : ARRAY OF CHAR);
		BEGIN
			FindNext("<", ptrXML^, 0, trouve, k);
			ptrXML^[k] := 0C;
			Assign(ptrXML^, dans);
			ptrXML^[k] := '<';
			ptrXML := ADR(ptrXML^[k]);
		END ValeurXml;
	BEGIN
		j := -1;

		LOOP
			FindNext(">", ptrXML^, 0, trouve, i);
			IF NOT trouve THEN EXIT END;
			INC(i);
			sos := ptrXML^[i];
			ptrXML^[i] := 0C;
			tagLu := ReadXMLTag(ptrXML^, ch_clauses_info);
			ptrXML^[i] := sos;
			ptrXML := ADR(ptrXML^[i]);

			IF (tagLu = invalid_tag) AND (tagLu = channel_information_data) THEN
				CONTINUE
			ELSIF (tagLu = channel_information) THEN
				INC(j);
				CONTINUE
			END;

			ValeurXml(valeurLue);

			WITH informationChunks.channelData^[j] DO
				CASE tagLu OF
					ch_name :
						Assign(valeurLue, name)

				| internal_name :
						Assign(valeurLue, internalName)

				| ch_color :

				| ch_unit :
						CASE valeurLue[0] OF
							'V' : unit := volt
						| 'g' : unit := g
						| 'd' : unit := deg_per_seg
						| 'u' : unit := u_tesla
						END;

				| channel_type :
						CASE valeurLue[0] OF
							'R' : channelType := ch_random_data_channel
						| 'C' : channelType := ch_calculated_time_channel
						| 'M' : channelType := ch_monotonic_data_channel
						END;

				| assigned_time_channel_index :
						StrToInt(valeurLue, assignedTimeChannelIndex, res)

				| data_type :
						CASE valeurLue[0] OF
							'F' : dataType := ch_float
						| 'D' : dataType := ch_double
						| 'I' :
								IF valeurLue[3] = '1' THEN
									dataType := ch_int16
								ELSE
									dataType := ch_int32
								END
						END

				| data_index :
						StrToInt(valeurLue, dataIndex, res)

				| start_time :
						Assign(valeurLue, startTime)

				| time_increment :
						timeIncrement := ValueReal(valeurLue)

				| range_min :
						rangeMin := ValueReal(valeurLue)

				| range_max :
						rangeMax := ValueReal(valeurLue)

				| internal_gain :
						internalGain := ValueReal(valeurLue)

				| internal_offset :
						internalOffset := ValueReal(valeurLue)

				| external_gain :
						externalGain := ValueReal(valeurLue)

				| external_offset :
						externalOffset := ValueReal(valeurLue)

				| per_channel_sample_rate :
						perChannelSampleRate := ValueReal(valeurLue)

				| requested_per_channel_sample_rate :
						requestedPerChannelSampleRate := ValueReal(valeurLue)

				| per_channel_base_frequency_divider :
						perChannelBaseFrequencyDivider := ValueReal(valeurLue)

				| physical_channel_number :
						StrToInt(valeurLue, physicalChannelNumber, res)

				| system_channel_number :
						StrToInt(valeurLue, systemChannelNumber, res)

				| uses_sensor_values :
						usesSensorValues := (valeurLue[0] = 'T')

				| start_trigger :
						Assign(valeurLue, startTrigger)

				| stop_trigger :
						Assign(valeurLue, stopTrigger)

				| requested_number_of_samples :
						StrToInt(valeurLue, requestedNumberOfSamples, res)

				ELSE
					(* invalid tag! *)
				END (* CASE tagLu*)
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


(* HPFSeekToChunk *)
PROCEDURE HPFSeekToChunk(CONST hpfFile : HPF_FILE; chunkId : HPF_SET_CHUNK_ID;
	VAR startingPos : FilePos) : BOOLEAN;
VAR
	chunkPos : FilePos;
	readChunk : HPF_CHUNK;
	lu : CARDINAL;
	p : POINTER TO CARDINAL;
BEGIN
	(* Chunks are alligned to 64kiB, each chunk position should be a multiple of 64k (10000h) *)
	p := ADR(startingPos);
	IF (p^ MOD 10000h) # 0 THEN
		p^ := p^ + (10000h - (p^ MOD 10000h));
	END;

	SetPos(hpfFile.chanId, startingPos);
	chunkPos := startingPos;

	LOOP
		RawRead(hpfFile.chanId, ADR(readChunk), 16, lu); (* read chunkId and chunkSize *)

		IF ReadResult(hpfFile.chanId) = endOfInput THEN
			EXIT
		END;

		IF readChunk.chunkId IN chunkId THEN
			SetPos(hpfFile.chanId, chunkPos);
			startingPos := chunkPos;
			RETURN TRUE
		END;

		chunkPos := NewPos(hpfFile.chanId, 1, readChunk.chunkSize, chunkPos);
		SetPos(hpfFile.chanId, chunkPos)
	END;

	RETURN FALSE
END HPFSeekToChunk;


(* HPFReadIndex *)
PROCEDURE HPFReadIndex(VAR hpfFile : HPF_FILE) : BOOLEAN;
VAR
	chunkPos : FilePos;
	lu : CARDINAL;
BEGIN
	chunkPos := StartPos(hpfFile.chanId);

	IF NOT HPFSeekToChunk(hpfFile, HPF_SET_CHUNK_ID{ch_index}, chunkPos) THEN
		RETURN FALSE
	END;

	RawRead(hpfFile.chanId, ADR(hpfFile.index), 24, lu);

	NEW(hpfFile.index.event, hpfFile.index.indexCount - 1);

	RawRead(hpfFile.chanId, hpfFile.index.event, hpfFile.index.indexCount * 40, lu);
	SetPos(hpfFile.chanId, NewPos(hpfFile.chanId, 1, hpfFile.index.chunkSize, chunkPos));

	RETURN TRUE
END HPFReadIndex;


(* HPFBuildIndex *)
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


(* HPFOpenFile *)
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


(* HPFOpenFile *)
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


(* HPFCloseFile *)
PROCEDURE HPFCloseFile(VAR hpfFile : HPF_FILE);
BEGIN
	Close(hpfFile.chanId);
	DISPOSE(hpfFile.index.event);
	DISPOSE(hpfFile.channelInformation.channelData);
	DISPOSE(hpfFile.data.channelDescriptor)
END HPFCloseFile;


(* HPFReadAtTime *)
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


(* HPFReadData *)
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
