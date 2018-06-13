package cz.crcs.mkq.data;

import java.io.*;
import java.nio.file.FileSystemNotFoundException;
import java.util.HashMap;
import java.util.InvalidPropertiesFormatException;
import java.util.Map;
import java.util.TreeMap;
import java.util.function.BiConsumer;

public class Main {

    private static final String SEPARATOR = ";";
    private static final int MODULUS_INDEX = 1;

    public static void main(String[] args) throws FileNotFoundException {
        if (args.length < 4) {
            System.err.println("Arguments: rootDirectory outputFile sourceToIdPath sourceToGroupPath [maxKeys [skipKeys]]");
            return;
        }
        File root = new File(args[0]);
        OutputStreamWriter output = new OutputStreamWriter(new BufferedOutputStream(new FileOutputStream(new File(args[1]))));
        File sourceToIdPath = new File(args[2]);
        File sourceToGroupPath = new File(args[3]);
        int maxKeyCount = 0;
        int skipKeys = 0;
        if (args.length > 4) {
            maxKeyCount = Integer.valueOf(args[4]);
            if (args.length > 5) {
                skipKeys = Integer.valueOf(args[5]);
            }
        }
        process(root, output, sourceToIdPath, sourceToGroupPath, maxKeyCount, skipKeys);
    }

    public static void process(File root, OutputStreamWriter output,
                               File sourceToIdPath, File sourceToGroupPath, int maxKeyCount, int skipKeys) {
        if (!root.isDirectory() || !root.exists()) throw new IllegalArgumentException("root not a directory or does not exist");
        File[] directories = root.listFiles(File::isDirectory);
        if (null == directories) throw new IllegalArgumentException("root contains no directories");
        Map<String, Integer> sourceToId = readMappingFile(sourceToIdPath);
        Map<String, Integer> sourceToGroup = readMappingFile(sourceToGroupPath);
        Map<Integer, Integer> sourceToSkipped = new HashMap<>();
        Map<Integer, Integer> sourceToOutput = new HashMap<>();
        try {
            output.write("modulus;group;source\n");
        } catch (IOException e) {
            throw new IllegalArgumentException(e);
        }
        for (File sourceTypeDirectory : directories) {
            File[] sourceDirectories = sourceTypeDirectory.listFiles(File::isDirectory);
            if (null == sourceDirectories) throw new IllegalArgumentException("directory contains no directories");
            for (File sourceDirectory : sourceDirectories) {
                System.out.println(sourceDirectory.getName());
                Integer id = sourceToId.get(sourceDirectory.getName());
                Integer group = sourceToGroup.get(sourceDirectory.getName());
                if (id == null || group == null) {
                    System.err.println(String.format("source or group undefined %s", sourceDirectory));
                    continue;
                }
                int skipped = sourceToSkipped.getOrDefault(id, 0);
                int printed = sourceToOutput.getOrDefault(id, 0);
                int maxSkip = skipKeys - skipped;
                int maxPrint = maxKeyCount - printed;
                int processed = processSingle(sourceDirectory, output, id, group, maxPrint, maxSkip);
                if (processed < maxSkip) {
                    skipped += processed;
                } else {
                    printed += processed - maxSkip;
                    skipped = skipKeys;
                }
                sourceToSkipped.put(id, skipped);
                sourceToOutput.put(id, printed);
            }
        }

        sourceToSkipped.forEach((id, skipped) -> {if (skipped < skipKeys)
            System.err.println(String.format("Not enough keys when skipping group %d: only %d skipped", id, skipped));});
        sourceToOutput.forEach((id, printed) -> {if (printed < maxKeyCount)
            System.err.println(String.format("Not enough keys when printing group %d: only %d printed", id, printed));});

        try {
            output.close();
        } catch (IOException e) {
            throw new IllegalArgumentException(e);
        }
    }

    private static int processSingle(File sourceDirectory, OutputStreamWriter output,
                                      Integer sourceId, Integer groupId, int maxKeyCount, int skipKeys) {
        if (null == sourceId || null == groupId) {
            throw new IllegalArgumentException("source or group undefined");
        }
        if (!sourceDirectory.isDirectory() || !sourceDirectory.exists()) throw new IllegalArgumentException(
                String.format("%s does not exist or not a directory", sourceDirectory.getName()));
        File[] files = sourceDirectory.listFiles(File::isFile);
        if (null == files) throw new IllegalArgumentException(String.format("%s contains no directories", sourceDirectory.getName()));
        int outputKeyCount = 0;
        int skippedKeyCount = 0;
        for (File file : files) {
            boolean headerSkipped = false;
            try (LineNumberReader reader = new LineNumberReader(new InputStreamReader(new FileInputStream(file)))) {
                String line;
                while (null != (line = reader.readLine())) {
                    if (!headerSkipped) {
                        headerSkipped = true;
                        continue;
                    }
                    if (skippedKeyCount < skipKeys) {
                        skippedKeyCount++;
                        continue;
                    }
                    String[] split = line.split(SEPARATOR);
                    StringBuilder builder = new StringBuilder();
                    builder.append(split[MODULUS_INDEX]);
                    builder.append(SEPARATOR);
                    builder.append(groupId);
                    builder.append(SEPARATOR);
                    builder.append(sourceId);
                    builder.append("\n");
                    if (outputKeyCount < maxKeyCount) {
                        output.write(builder.toString());
                        outputKeyCount++;
                    } else {
                        break;
                    }
                }
            } catch (IOException e) {
                throw new IllegalArgumentException(e);
            }
        }
        return outputKeyCount + skippedKeyCount;
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
