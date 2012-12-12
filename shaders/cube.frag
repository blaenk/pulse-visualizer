#version 110

// inspired by http://duriansoftware.com/joe/An-intro-to-modern-OpenGL.-Chapter-2.2:-Shaders.html

uniform float bar_number;
uniform float spectrum;

void main() {
  gl_FragColor = mix(
    vec4(1.0, 1.0, 0.0, 1.0),
    vec4(1.0, 0.0, 0.0, 1.0),
    spectrum
  );
}
