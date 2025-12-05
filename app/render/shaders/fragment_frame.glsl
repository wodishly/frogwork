#version 410

precision highp sampler2D;

in vec4 v_position;
out vec4 f_color;

uniform float u_stricken;


bool alongEdge(float f, float threshold) {
  return abs(f) < threshold || abs(f-1) < threshold;
}

bool alongEdges(vec4 v, float threshold) {
  return int(alongEdge(v.x, threshold))
       + int(alongEdge(v.y, threshold))
       + int(alongEdge(v.z, threshold))
      >= 2;
}

void main() {
  float threshold = 1.0/40;
  if (!alongEdges(v_position, threshold))
    discard;

  f_color = u_stricken > 0.0 ? vec4(1, 0, 0, 1) : vec4(0, 1, 0, 1);
}
