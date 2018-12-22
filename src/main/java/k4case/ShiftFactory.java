package k4case;

import org.joda.time.DateTime;

public interface ShiftFactory {
    TestScenario.Shift create(String id, DateTime startTime, DateTime endTime, TestScenario.WorkPlace workPlace, TestScenario.Worker foreman,
                              TestScenario.Worker[] workers, TestScenario.Worker[] standby, scala.collection.immutable.Map<TestScenario.Worker, String> assignment);
}
