package baboon.runtime.shared;

public record BaboonMethodId(String serviceName, String methodName) {
    @Override
    public String toString() {
        return serviceName + "#" + methodName;
    }
}
