package k4case;

import java.io.IOException;
import java.nio.file.Files;

import java.nio.file.Paths;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.yaml.snakeyaml.Yaml;
import scala.collection.immutable.Set;
import tcof.traits.map2d.Node;
import tcof.traits.map2d.Position;

public class EntityReader {

    private static Position readPosition(Object o) {
        if (! (o instanceof List)) {
            throw new RuntimeException("Position is not a list");
        }
        List<Integer> s = (List<Integer>) o;
        return new Position(s.get(0), s.get(1));
    }

    private static Set<String> readCapabilities(Object o) {
        if (! (o instanceof List)) {
            throw new RuntimeException("Capabilities are not a list");
        }

        List<String> s = (List<String>) o;
        java.util.Set<String> set = s.stream().collect(Collectors.toSet());

        return scala.collection.JavaConverters.asScalaSet(set).toSet();
    }

    public static Set<TestScenario.Worker> readWorkersFromYaml(TestScenario testScenario, String fname, WorkerFactory factory) throws IOException {
        Yaml yaml = new Yaml();
        Map<String, List<Map<String, Object>>> data = yaml.load(Files.newBufferedReader(Paths.get(fname)));

        java.util.Set<TestScenario.Worker> workers = data.get("employees").stream().map(e ->
                factory.create(((Integer)e.get("id")).toString(), readPosition(e.get("position")), readCapabilities(e.get("capabilites")))).
                collect(Collectors.toSet());
        return scala.collection.JavaConverters.asScalaSet(workers).toSet();

    }

    public static Set<TestScenario.Shift> readShiftsFromYaml(TestScenario testScenario, String fname, Set<TestScenario.Worker> workers, ShiftFactory factory) throws IOException {
        return null;
    }

    public static void readMapFromYaml(TestScenario testScenario, String fname) throws IOException {
        Yaml yaml = new Yaml();
        Map<String, Map<String, List<Map<String, Object>>>> data = yaml.load(Files.newBufferedReader(Paths.get(fname)));

        Map<String, List<Map<String, Object>>> map = data.get("map");
        List<Map<String, Object>> points = map.get("points");

        Map<String, Node> pointsMap = points.stream().map(p ->
            new java.util.AbstractMap.SimpleImmutableEntry<String, Node>((String) p.get("id"), testScenario.map().addNode(readPosition(p.get("position"))))
        ).collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));

        List<Map<String, Object>> edges = map.get("edges");
        edges.forEach(edge -> {
            String n1 = (String) edge.get("nodeIn");
            String n2 = (String) edge.get("nodeIn");
            testScenario.map().addDirectedEdge(pointsMap.get(n1), pointsMap.get(n2));
            if ((Boolean) edge.get("bidirectional")) {
                testScenario.map().addDirectedEdge(pointsMap.get(n2), pointsMap.get(n1));
            }
        });
    }
}
