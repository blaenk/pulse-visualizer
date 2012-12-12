#version 110

uniform float bar_number;
uniform float spectrum;

attribute vec3 position;

void main() {
  vec3 scaledPosition = position;
  scaledPosition.y *= 300.0 * spectrum;
  scaledPosition.x += scaledPosition.x - 0.7 + (bar_number * 0.2);
  gl_Position = vec4(scaledPosition, 1.0);
}
