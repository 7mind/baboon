plugins {
    kotlin("jvm") version "2.1.0"
}

group = "io.septimalmind"
version = "0.1.0-SNAPSHOT"

repositories {
    mavenCentral()
}

dependencies {
    implementation("org.jetbrains.kotlinx:kotlinx-serialization-json:1.8.0")

    testImplementation(kotlin("test"))
    testImplementation("org.junit.jupiter:junit-jupiter:5.11.4")
    testRuntimeOnly("org.junit.platform:junit-platform-launcher")
}

tasks.test {
    useJUnitPlatform()
}

kotlin {
    compilerOptions {
        jvmTarget.set(org.jetbrains.kotlin.gradle.dsl.JvmTarget.JVM_21)
        // allWarningsAsErrors is intentionally NOT set for the MCP mux overlay test:
        // the generated code may contain deprecation suppression annotations that
        // differ from the regular test model.
    }
}

java {
    sourceCompatibility = JavaVersion.VERSION_21
    targetCompatibility = JavaVersion.VERSION_21
}

sourceSets {
    main {
        kotlin {
            srcDir("src/main/kotlin/generated-main")
        }
    }
    test {
        kotlin {
            // Only include the MCP mux test overlay; exclude the regular-model runtime
            // tests (src/test/kotlin/runtime/) that reference types absent in
            // the mcp-mux-stub-ok model.
            setSrcDirs(listOf("src/test/kotlin/mcpmux"))
        }
    }
}
