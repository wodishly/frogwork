#version 410

precision highp sampler2D;

in vec4 v_position;
out vec4 f_color;


bool near(float f, float g, float threshold) {
    return abs(f - g) < threshold;
}

bool alongEdge(float f, float threshold) {
  return near(f, 0, threshold) || near(f, 1, threshold);
}

bool alongEdges(vec4 v, float threshold) {
  return int(alongEdge(v.x, threshold))
       + int(alongEdge(v.y, threshold))
       + int(alongEdge(v.z, threshold))
      >= 2;
}

bool alongSpit(vec4 v, float threshold) {
  return false;//near(v.x, v.y, threshold) && near(v.y, v.z, threshold) && near(v.x, v.z, threshold);
}

void main() {
  float threshold = 1.0/40;
  if (alongSpit(v_position, 0.2) || alongEdges(v_position, threshold)) {
    f_color = vec4(0, 1, 0, 1);
  } else {
    discard;
  }
}
