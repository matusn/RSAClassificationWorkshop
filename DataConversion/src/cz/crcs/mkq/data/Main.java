package cz.crcs.mkq.data;

import java.io.*;
import java.nio.file.FileSystemNotFoundException;
import java.util.InvalidPropertiesFormatException;
import java.util.Map;
import java.util.TreeMap;

public class Main {

    private static final int COLUMN_COUNT = 4;
    private static final int MODULUS_LENGTH_INDEX = 2;
    private static final String SEPARATOR = ";";

    public static void main(String[] args) throws FileNotFoundException {
        if (args.length != 5) {
            System.err.println("Arguments: rootDirectory outputFile sourceToIdPath sourceToGroupPath keyLengthToIdPath");
            return;
        }
        File root = new File(args[0]);
        OutputStreamWriter output = new OutputStreamWriter(new BufferedOutputStream(new FileOutputStream(new File(args[1]))));
        File sourceToIdPath = new File(args[2]);
        File sourceToGroupPath = new File(args[3]);
        File keyLengthToIdPath = new File(args[4]);
        process(root, output, sourceToIdPath, sourceToGroupPath, keyLengthToIdPath);
    }

    public static void process(File root, OutputStreamWriter output,
                               File sourceToIdPath, File sourceToGroupPath, File keyLengthToIdPath) {
        if (!root.isDirectory() || !root.exists()) throw new IllegalArgumentException("root not a directory or does not exist");
        File[] directories = root.listFiles(File::isDirectory);
        if (null == directories) throw new IllegalArgumentException("root contains no directories");
        Map<String, Integer> sourceToId = readMappingFile(sourceToIdPath);
        Map<String, Integer> sourceToGroup = readMappingFile(sourceToGroupPath);
        Map<String, Integer> keyLengthToId = readMappingFile(keyLengthToIdPath);
        try {
            output.write("nmsb;nlsb;nblen;nmod3;group;source\n");
        } catch (IOException e) {
            throw new IllegalArgumentException(e);
        }
        for (File sourceTypeDirectory : directories) {
            File[] sourceDirectories = sourceTypeDirectory.listFiles(File::isDirectory);
            if (null == sourceDirectories) throw new IllegalArgumentException("directory contains no directories");
            for (File sourceDirectory : sourceDirectories) {
                System.out.println(sourceDirectory.getName());
                processSingle(sourceDirectory, output, sourceToId.get(sourceDirectory.getName()),
                        sourceToGroup.get(sourceDirectory.getName()), keyLengthToId);
            }
        }
    }

    private static void processSingle(File sourceDirectory, OutputStreamWriter output,
                                      Integer sourceId, Integer groupId, Map<String, Integer> keyLengthToId) {
        if (null == sourceId || null == groupId) throw new IllegalArgumentException("soruce or group undefined");
        if (!sourceDirectory.isDirectory() || !sourceDirectory.exists()) throw new IllegalArgumentException(
                String.format("%s does not exist or not a directory", sourceDirectory.getName()));
        File[] files = sourceDirectory.listFiles(File::isFile);
        if (null == files) throw new IllegalArgumentException(String.format("%s contains no directories", sourceDirectory.getName()));
        for (File file : files) {
            boolean headerSkipped = false;
            try (LineNumberReader reader = new LineNumberReader(new InputStreamReader(new FileInputStream(file)))) {
                String line;
                while (null != (line = reader.readLine())) {
                    if (!headerSkipped) {
                        headerSkipped = true;
                        continue;
                    }
                    String[] split = line.split(SEPARATOR);
                    if (split.length != COLUMN_COUNT) continue;
                    Integer replacementLength = keyLengthToId.get(split[MODULUS_LENGTH_INDEX]);
                    if (null == replacementLength) {
                        System.err.println(String.format("unknown length %d", split[MODULUS_LENGTH_INDEX]));
                        continue;
                    }
                    split[MODULUS_LENGTH_INDEX] = replacementLength.toString();
                    StringBuilder builder = new StringBuilder();
                    for (String field : split) {
                        builder.append(field);
                        builder.append(SEPARATOR);
                    }
                    builder.append(groupId);
                    builder.append(SEPARATOR);
                    builder.append(sourceId);
                    builder.append("\n");
                    output.write(builder.toString());
                }
            } catch (IOException e) {
                throw new IllegalArgumentException(e);
            }
        }
    }

    private static Map<String, Integer> readMappingFile(File mappingFile) {
        try (LineNumberReader reader = new LineNumberReader(new InputStreamReader(new FileInputStream(mappingFile)))) {
            Map<String, Integer> labelToId = new TreeMap<>();
            String line;
            while (null != (line = reader.readLine())) {
                String[] split = line.split(";");
                if (split.length != 2) throw new IllegalArgumentException(
                        String.format("Bad mapping in file %s on line %d", mappingFile.getName(), reader.getLineNumber()));
                labelToId.put(split[0], Integer.valueOf(split[1]));
            }
            return labelToId;
        } catch (IOException e) {
            throw new IllegalArgumentException(e);
        }
    }
}
