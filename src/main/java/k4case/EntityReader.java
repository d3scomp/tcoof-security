package k4case;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.yaml.snakeyaml.Yaml;
import scala.Enumeration;
import scala.collection.immutable.Set;
import tcof.traits.map2d.Position;

public class EntityReader {

    private static Position readPosition(Object o) {
        if (! (o instanceof List)) {
            throw new RuntimeException("Position is not a list");
        }
        List<Integer> s = (List<Integer>) o;
        return new Position(s.get(0), s.get(1));
    }

    private static Set<Enumeration.Value> readCapabilities(Object o) {
        if (! (o instanceof List)) {
            throw new RuntimeException("Capabilities are not a list");
        }

        List<String> s = (List<String>) o;
        java.util.Set<Enumeration.Value> set = s.stream().map(str -> {
            switch (str) {
                case "A": return TestScenario.Capability$.MODULE$.CAP_A();
                case "B": return TestScenario.Capability$.MODULE$.CAP_B();
                case "C": return TestScenario.Capability$.MODULE$.CAP_C();
                case "D": return TestScenario.Capability$.MODULE$.CAP_D();
                case "E": return TestScenario.Capability$.MODULE$.CAP_E();
                default: return TestScenario.Capability$.MODULE$.CAP_UNKNOWN();
            }}).collect(Collectors.toSet());

        return scala.collection.JavaConverters.asScalaSet(set).toSet();
    }

    public static Set<TestScenario.Worker> readWorkersFromYaml(TestScenario testScenario, String fname) throws IOException {
        Yaml yaml = new Yaml();
        Map<String, List<Map<String, Object>>> data = yaml.load(Files.newBufferedReader(Paths.get(fname)));

        java.util.Set<TestScenario.Worker> workers = data.get("employees").stream().map(e ->
                testScenario.new Worker(((Integer)e.get("id")).toString(), readPosition(e.get("position")), readCapabilities(e.get("capabilities")))).
                collect(Collectors.toSet());
        return scala.collection.JavaConverters.asScalaSet(workers).toSet();

    }
}
