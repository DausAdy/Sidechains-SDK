package com.horizen.evm.utils;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonDeserializer;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;

import java.io.IOException;
import java.util.Arrays;

public class Hash {
    public static final int LENGTH = 32;
    private final byte[] bytes;

    public Hash(byte[] bytes) {
        if (bytes.length != LENGTH) {
            throw new IllegalArgumentException("hash must have a length of " + LENGTH);
        }
        this.bytes = bytes;
    }

    public byte[] toBytes() {
        return Arrays.copyOf(bytes, LENGTH);
    }

    public static class Serializer extends JsonSerializer<Hash> {
        @Override
        public void serialize(
            Hash address, JsonGenerator jsonGenerator, SerializerProvider serializerProvider
        ) throws IOException {
            jsonGenerator.writeString("0x" + Converter.toHexString(address.bytes));
        }
    }

    public static class Deserializer extends JsonDeserializer<Hash> {
        @Override
        public Hash deserialize(JsonParser jsonParser, DeserializationContext deserializationContext)
            throws IOException {
            var text = jsonParser.getText();
            if (!text.startsWith("0x")) {
                throw new IOException("hash must be prefixed with 0x");
            }
            return new Hash(Converter.fromHexString(text.substring(2)));
        }
    }
}
