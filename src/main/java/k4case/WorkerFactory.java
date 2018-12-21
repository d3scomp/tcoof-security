package k4case;

public interface WorkerFactory {
    TestScenario.Worker create(String id, tcof.traits.map2d.Position pos, scala.collection.immutable.Set<String> caps);
}
